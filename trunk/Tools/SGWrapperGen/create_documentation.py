#!/usr/bin/env python
# encoding: utf-8
"""

Created by Andrew Cain on 2010-03-19.
Updates (HTML+CSS+TOC) by Clinton Woodward
Copyright (c) 2010 Swinburne University of Technology. All rights reserved.
"""

#TODO: use "tags" (instead of "doc_group" for  )

import logging, sys, re, time, subprocess

from sg import parser_runner
from sg.sg_cache import logger, find_or_add_file
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter
from sg.file_writer import FileWriter

from lang_c import create_c_code_for_file
from lang_pas import create_pas_code_for_file

#==============================================================================
# Settings and global data ...
#==============================================================================

def get_svn_version():
    try:
        lines = subprocess.Popen("svn info",shell=True, stdout=subprocess.PIPE).stdout.readlines()
        print "svn info: ", lines[4]
        result = lines[4].split()[1].strip() # eg. "Revision: 12345"
    except:
        print
        result = "1284" # rough guess...
    return result

OUT_PATH = "../../Generated/Documentation"

SVN_VERSION = get_svn_version()

_google_base_url = "http://code.google.com/p/swingamesdk/source/browse/trunk/CoreSDK/src/"

# List the pascal types that we do not want to document and link to
_nolink_types = (
    'Single', 'String', 'Boolean', 'Longint', 'Byte', 'UInt32', 'Longword', 'UInt16',
    'PSDL_Surface', 'PMix_Music', 'PMix_Chunk', 'Pointer'
)

def source_url(text):
    '''Break up a standard source code line string and return a URL to the code.
    ie. url="http://code.google. ... /[filename]?r=[SVN_VERSION]#[line_no]"
    '''
    bits = text.split()
    line_no = bits[3]
    fname = bits[5].split('/')[-1].strip()
    return _google_base_url + fname + "?r=" + SVN_VERSION + '#' + line_no    


#==============================================================================
# Document writer class to consistently format API html files
#==============================================================================

class APIDocWriter(object):
    '''Base class for presentation API documentation providing html template
    features including consistent header/footer details, toc and style links
    '''
    
    _html = '''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" 
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<title>%(title)s</title>
<link rel="stylesheet" href="style.css" type="text/css" />
%(css)s
</head>
<body>
%(topnav)s

<h1>%(title)s</h1>

%(toc)s

%(desc)s

%(body)s

<div id="footer">
Generated %(datetime)s for svn version %(svnversion)s
</div>
</body>
</html>

'''
    def __init__(self):
        # keep track of standard document information and unique content
        self.title = ''
        self.css = ''
        self.topnav = [
            ('index.html', 'API Index'),
            ('identifiers.html', 'Identifiers'),
            ('Types.html', 'Types')
        ]
        self.toc = []
        self.desc = ''
        self.body = []
        # bind the body append/extend methods to this object
        self.append = self.body.append
        self.extend = self.body.extend

    def savetofile(self, filename):
        file_writer = FileWriter(OUT_PATH + '/' + filename)        
        file_writer.write(self._html % {
            # header/title details
            'title': self.title,
            'css': self.format_css(),
            # navigation features
            'topnav': self.format_topnav(filename),
            'toc': self.format_toc(),
            # content
            'desc': self.format_desc(),
            'body': '\n'.join(self.body),
            # footer details
            'datetime': time.strftime('%Y-%m-%d %H:%M:%S'), 
            'svnversion': SVN_VERSION,
        } )
        file_writer.close()

    def format_topnav(self, filename):
        tmp = '<div id="topnav">\n<ul>\n%(items)s\n</ul>\n</div>'
        if self.topnav:
            items = []
            for f, title in self.topnav:
                if f == filename:
                    items.append('<li><a href="%s" class="current">%s</a></li>' % (f,title))
                else:
                    items.append('<li><a href="%s">%s</a></li>' % (f,title))
            return tmp % {'items': ' '.join(items) } 
        else:
            return ''
    
    def format_css(self):
        if self.css:
            return '<style type="text/css">%s\n</style>' % self.css
        else:
            return ''
    
    def format_toc(self):
        '''Convert toc list of tuples (name, uname) to list items. 
        Only 1 unique name is presented in the toc for brevity.
        '''
        toc = '<div id="toc">\n<ul>\n%(toc)s\n</ul>\n</div>\n'
        tmp = []
        last = ''
        for title, uname in self.toc:
            if title != last:
                last = title
                tmp.append('<li><a href="#%s" title="%s">%s</a></li>' % (uname, uname, title))
        return toc % {'toc': '\n'.join(tmp) }

    def format_desc(self):
        tmp = '<h2 id="desc">Description</h2>\n<p>\n%(desc)s\n</p>\n'
        if self.desc:
            return tmp % {'desc': self.desc }
        else:
            return ''        

    def h2(self, text, id=''):
        if id == '':
            self.body.append('<h2>%s</h2>' % text)
        else:
            self.body.append('<h2 id="%s">%s</h2>' % (id, text))

    def h3(self, text, id=''):
        if id == '':
            self.body.append('<h3>%s</h3>' % text)
        else:
            self.body.append('<h3 id="%s">%s</h3>' % (id, text))


#==============================================================================
# Collect all the identifier details we need (methods, types)
#==============================================================================

class IdentifierCollector(object):
   
    def __init__(self):

        # Regular expressions for catching `linked-code` and ``code-formated`` text
        self.p_code = re.compile(r'``(.*?)``')
        self.p_kind = re.compile(r'`(.*?)`')
        # Keep all the identifiers we need...
        ids = self.ids = {
            'files': {},
            'methods': {},
            'umethods': {},
            'types': {},
            'structs': {},
            'enums': {},
            'classes': {},
            'consts': {}, #TODO
        }
        
        # Load the signatures from other languages...
        parser_runner.visit_all_units(create_c_code_for_file)
        parser_runner.visit_all_units(create_pas_code_for_file)
        
        # Gather identifier details
        parser_runner.visit_all_units(self._file_visitor)
        
        # Build link-calls back to method parameters
        for key, m in ids['umethods'].items():
            if m.params:
                for p in m.params:
                    # only keep the special types, not Longint etc
                    if p.data_type.name in ids['types']:
                        ids['types'][p.data_type.name]['used_by'][m.uname] = m

    def link_type(self, name):
        '''Convert text to hyperlinked version if type is known and special. '''
        name = name.strip()
        if name in _nolink_types: 
            return '<span class="code">%s</span>' % name
        elif '[' in name: # special array type handling...
            pos = name.index('[')
            return '%s<span class="code">%s</span>' % (self.link_type(name[:pos]), name[pos:]) 
        elif name in self.ids['umethods']:
            doc_url = self.ids['umethods'][name]['doc_url']
            return '<a class="code" href="%s">%s</a>' % (doc_url, name) 
        elif name in self.ids['types']:
            doc_url = self.ids['types'][name]['doc_url']
            return '<a class="code" href="%s">%s</a>' % (doc_url, name) 
        else:
            print '## unknown id:', name
            return '<span class="code">%s</span>' % name
        
    def format_text(self, text):
        '''Convert text to valid xhtml+css markup'''
        # Convert double-ticks ``-`` into code format (no hypertext link)
        text = self.p_code.sub(r'<span class="code">\1</span>' , text)
        # Convert single-ticks `-` into kind (identifier) links
        def matcher(m):
            # m is a Match instance - return the text to replace the matched text with
            return self.link_type(m.groups()[0])             
        text = self.p_kind.sub(matcher, text)
        # Reformat paragraph breaks for pretty presentation
        if len(text.strip()) > 0:
            lines= text.split('\n')
            for i, line in enumerate(lines):
                if line.strip() == '': lines[i] = '</p><p>' 
            text = '\n'.join(lines)
        # done
        return text

    def _file_visitor(self, the_file, other):
        if the_file.name in ['SGSDK']: return
        print '>> %s ... ' % (the_file.name)
        # Keep the filename for the index 
        self.ids['files'][the_file.name] = the_file 
        
        for m in the_file.members:
            if m.is_module:
                # procedures / functions
                m.visit_methods(self._method_visitor, None)
            elif m.is_class or m.is_struct or m.is_enum or m.is_type:
                # class/struct/enum/type stuff
                self._type_visitor(m, None)   

    def _method_visitor(self, method, other):
        # keep the common and possibly overloaded name
        if method.name not in self.ids['methods']:
            self.ids['methods'][method.name] = []
        self.ids['methods'][method.name].append( method )
        # keep all unique (library) names
        if method.uname not in self.ids['umethods']:
            self.ids['umethods'][method.uname] = method
        # modify method to also keep the doc_url for us
        method.tags['doc_url'] = method.in_file.name + '.html#' + method.uname
    
    def _type_visitor(self, member, other):
        self.ids['types'][member.name] = member
        # modify member to keep track of who uses it
        member.tags['used_by'] = {} 
        # modify member to keep track of the doc_url for us
        member.tags['doc_url'] = 'Types.html#' + member.name
        
        # determine the group and keep it for later ...
        if member.is_enum: group = 'enums'
        elif member.is_class: group = 'classes'
        elif member.is_struct: group = 'structs'
        elif member.is_data_wrapper: return # eg. Color
        elif member.is_pointer_wrapper: return # eg. BitmapPtr
        elif member.wraps_array: return # BitmapArray
        else: #elif member.is_type:
            print '## is_type', member.uname
            return 
        self.ids[group][member.name] = member
        
        # if member.is_class or member.is_type or (member.is_struct and member.wraps_array):
        #     #convert to resource pointer
        #     if member.is_pointer_wrapper:
        #         # assert len(member.fields) == 1
        #         the_type = member.data_type
        #         other['header writer'].writeln('typedef %s;\n' % adapter_type_visitor(the_type, None) % member.lower_name)
        #     elif member.is_data_wrapper:
        #         assert len(member.fields) == 1
        #         the_type = member.fields['data'].data_type
        #         other['header writer'].writeln('typedef %s;\n' % adapter_type_visitor(the_type) % member.lower_name)
        #     elif member.wraps_array:
        #         assert len(member.fields) == 1
        #         the_type = member.fields['data'].data_type
        #         other['header writer'].writeln('typedef %s;\n' % adapter_type_visitor(the_type) % member.lower_name)
        #     elif member.data_type.is_procedure:
        #         assert member.data_type.method != None
        #         #typedef float(*pt2Func)(float, float);
        #         m = member.data_type.method
        #         other['header writer'].writeln('typedef %s;\n' % adapter_type_visitor(member.data_type) % m.lower_name)
        #     else:
        #         logger.error('CREATE C  : Unknown class type for %s', member.uname)
        #         assert False
        # elif member.is_struct:
        #     #typedef struct %s_struct { } struct;
        #     writer = other['header writer']
        #     writer.write('typedef struct { \n')
        #     for field in member.field_list:
        #         writer.writeln('    %s;' % adapter_type_visitor(field.data_type) % field.lower_name)
        #     writer.writeln('} %s;\n' % member.lower_name)
        # elif member.is_enum:
        #     #enum id { list }
        #     other['header writer'].write('typedef enum { \n    ')
        #     other['header writer'].write( ',\n    '.join([wrapper_helper.upper_name(v) for v in member.values]))
        #     other['header writer'].writeln('\n} %s;\n' % member.lower_name)    
        #pass
      


#==============================================================================
# Present each unit (module) as a pretty HTML document.
#==============================================================================

class UnitPresenter(object):

    def __init__(self, idcollection):
        self.doc = None
        self.idcollection = idcollection
        parser_runner.visit_all_units(self.file_visitor)

    def lead_trim(self, text):
        '''Remove leading "-" or ":" from text as it sometimes appears in parameter 
        description text.'''
        text = text.strip()
        if len(text) > 0 and text[0] in ['-',':']:
            text = text[1:].strip()
        return text

    def method_visitor(self, method, other):
        link_type = self.idcollection.link_type
        format_text = self.idcollection.format_text
        '''Format the current method details and store in the global body and toc lists '''
        # Keep the toc entry (name and unique ID for hyperlinks)
        self.doc.toc.append( (method.name, method.uname) )
        # Build up the parameters with formatted modifier terms if used
        tmp = []
        for p in method.params:
            if p.modifier:
                tmp.append("<span class='pmod'>%s</span> %s" % (p.modifier, p.name))
            else:
                tmp.append(p.name)
        param_txt = ', '.join(tmp)
        # Create the method signature
        if method.return_type != None:
            sig = '%s(%s) : %s' % (method.name, param_txt, link_type(str(method.return_type)))
        else:
            sig = '%s(%s)' % (method.name, param_txt)
            
        # Print the headings    
        tmp = '<div class="method" id="%(uname)s">\n<h3>%(name)s</h3>\n' + \
              '<p class="sig">%(sig)s</p>\n' + \
              '%(desc)s\n'
        desc = '' if method.doc.strip() == '' else '<p>%s</p>' % format_text(method.doc)
        self.doc.append(tmp % {'uname': method.uname, # unique id's for overloads
                               'name': method.name, 
                               'desc': desc, 
                               'sig': sig })
        
        # If parameters and/or return type details
        if len(method.params) > 0 or method.return_type:
            # START LIST    
            self.doc.append('<dl class="fields">')
            # PARAMETERS
            if len(method.params) > 0:
                self.doc.append('<dt>Parameters:</dt>\n<dd>\n<dl>')
                for p in method.params:
                    tmp = '<dt><span class="pname">%(pname)s</span> : <span class="ptype">%(ptype)s</span></dt>\n%(pdesc)s'
                    if len(self.lead_trim(p.doc).strip()) > 0:
                        pdesc = '<dd>%s</dd>' % format_text(self.lead_trim(p.doc))
                    else:
                        pdesc = ''
                    self.doc.append(tmp % {'pname': p.name, 
                                           'ptype': link_type(p.data_type.name), 
                                           'pdesc': pdesc })
                self.doc.append('</dl>\n</dd>')
            # RETURN TYPE DETAILS    
            if method.return_type:
                tmp = '<dt>Returns:</dt>\n' + \
                      '<dd><span class="rtype">%(rtype)s</span> : %(rdesc)s</dd>'
                self.doc.append(tmp % {'rtype': link_type(method.return_type.name), 
                                       'rdesc': format_text(self.lead_trim(method.returns)) })
            # SIGNATURES - by language (Andrew)
            if len(method.lang_data) > 0:
                self.doc.append('<dt>Signatures by Language:</dt>')
                lang_keys = method.lang_data.keys()
                lang_keys.sort()
                lang_map = {'c': 'C/C++', 'pas': 'Pascal'}
                for key in lang_keys:
                  if key == 'c':
                    bits = [ bit + ';' for bit in method.alias(key).signature.split(';')[:-1]]
                  else:
                    bits = [method.alias(key).signature]
                  for bit in bits:
                    self.doc.append('<dd><span class="langkey">%s:</span> <span class="code">%s</span></dd>' % (lang_map[key], bit.strip()))
            # END LIST
            self.doc.append('</dl>')
            
        # url = source_url(method.meta_comment_line_details),
        # self.doc.append(
        # '<p><a href="url" target="new" href="%s">Source URL</a></p>' % 
        # 
        # )

        #TODO: fix "side effects" comments into normal text
        #TODO: add @see details to 
        info = []
        # link to pascal source file
        info.append('<li><a target="new" href="%s">source code</a></li>' % source_url(method.meta_comment_line_details))
        # tags / document group details?
        if method.doc_group:
            info.append('<li>tags: %s</li>' % method.doc_group) 
        # library unique name for this method
        if method.uname != method.name:
            info.append('<li>lib name: <span class="code">%s</span></li>' % method.uname )

        # TODO: method of class details? show it...
        # <li>in_class: %(in_class)s</li>
        # <li>method_called: %(method_called)s</li>
        
        # Meta-details 
        self.doc.append('<div class="info">\n<ul>\n%s\n</ul>\n</div>' % '\n'.join(info))
        self.doc.append("\n</div>\n")    
    
    def file_visitor(self, the_file, other):
        # Don't do some files...
        if the_file.name in ['SGSDK', 'Types']: return
        #if the_file.name in ['SGSDK']: return
        
        # Create a new document to write to
        self.doc = APIDocWriter()
        self.doc.title = the_file.name
        self.doc.desc = self.idcollection.format_text(the_file.members[0].doc)
        
        # Here we go...
        print '>> %s ... ' % (the_file.name)
        for m in the_file.members:
            if m.is_module: 
                m.visit_methods(self.method_visitor, None)
                
        self.doc.savetofile(the_file.name+'.html')



#==============================================================================
# Present the Index page
#==============================================================================
class IndexPresenter(object):

    def __init__(self, idcollection):
        print "Creating index.html ...",
        doc = APIDocWriter()
        doc.title = "SwinGame API Documentation Index"
        # Describe the indentifier and types pages
        doc.append('''
        <dl>
            <dt><a href="identifiers.html">Identifiers</a></dt>
            <dd>An alphbetical list of all method names and type identifiers. 
            A text "search" in this page for key words might help you find just what 
            you need...</dd>
    
            <dt><a href="Types.html">Types</a></dt>
            <dd>Data type details so you know who contains what!</dd>
        </dl>
        ''')        
        # Build up the list of files as links
        doc.h2('Modules') 
        doc.append('<dl>')
        links = []
        format_text = idcollection.format_text
        for name, obj in idcollection.ids['files'].items():
            links.append("<dt><a href='%s.html'>%s</a></dt>" % (name, name))
            links.append("<dd>%s</dd>" % format_text(obj.members[0].doc))
        doc.extend(links)
        doc.append('</dl>')
        doc.savetofile('index.html')
        print 'Done.'       



#==============================================================================
# Create Identifiers Doc
#==============================================================================

def create_identifiers_doc(idcollection):
    ids = idcollection.ids
    link_type = idcollection.link_type
    format_text = idcollection.format_text
    # Create the identifiers.html document body content ...
    print 'Creating identifiers.html ...',
    doc = APIDocWriter()
    doc.title = 'Identifiers'
    doc.css = '''
#links li { 
    list-style-type: none; 
    display: block; 
    float: left; 
    width: 1.5em; 
    text-align: center;
    background: #eee
}

h2, h3 { clear: both; }
h3 { border-bottom: 1px solid #955 }
.methods li, #types li { list-style-type: none; }
#types li { display: inline; width: 20em; float: left }
'''
    doc.toc = [('Methods','Methods'),('Types','Types')]
    doc.h2('Methods','Methods')
        
    ## Methods 
    tmp = []
    links = []
    keys = ids['methods'].keys()
    keys.sort()
    current = ''
    for key in keys:
        if key[0] != current:
            if current != '':
                tmp.append('</ul>')
            current = key[0]
            tmp.append('<h3 id="'+current+'">'+current+'</h3>')
            tmp.append('<ul class="methods">')
            links.append(current)
            
        if len(ids['methods'][key]) > 1:
            unames = [ m.uname for m in ids['methods'][key] ]
            unames.sort()
            tmp.append( "<li>%s (%d): <ul>" % (key, len(unames)) )
            for n in unames:
                url = ids['umethods'][n]['doc_url']
                tmp.append( '<li><a href="%s">%s</a></li>' % (url,n) )  
            tmp.append('</ul></li>')
        else:
            url = ids['methods'][key][0]['doc_url'] # [0] == there is only one...
            tmp.append('<li><a href="%s">%s</a></li>' % (url,key) )
    tmp.append('</ul>')                                

    # Build a pretty alphabet of links for the top ...
    tmp2 = []
    for c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ':
        if c in links:
            tmp2.append('<a href="#%s">%s</a>' % (c,c))
        else:
            tmp2.append(c)
            
    # add the alpha list of links to the top
    doc.append('<ul id="links">\n<li>' + ('</li><li>'.join(tmp2)) + '</li></ul>')
    # add all the method details
    doc.extend(tmp)

    # Data Types
    doc.h2('Types','Types')
    doc.append('<ul id="typelist">')
    keys = ids['types'].keys()
    keys.sort()
    for key in keys:
        num = len(ids['types'][key]['used_by'])
        url = ids['types'][key]['doc_url']
        if num > 0:
            doc.append('<li><a href="%s">%s</a> (%d)</li>' % (url, key, num))
        else:
            doc.append('<li class="code">%s (%d)</li>' % (key, num))
    doc.append('</ul>')
    
    doc.savetofile('identifiers.html')
    print 'Done.'


#==============================================================================
# Create Types Doc
#==============================================================================

def create_types_doc(idcollection):
    ids = idcollection.ids
    link_type = idcollection.link_type
    format_text = idcollection.format_text
    # Create the single types files with all the types 
    print 'Creating Types.html ...'
    doc = APIDocWriter()
    doc.title = 'Types'
    doc.desc = 'DESCRIPTION'
    doc.h2('Details')        
    
    keys = ids['types'].keys()
    keys.sort()
    for key in keys:
        obj = ids['types'][key]
        # keep a TOC entry
        doc.toc.append((key, key))
        # Print the headings    
        tmp = '<div class="type" id="%(name)s">\n<h3>%(name)s</h3>\n%(desc)s\n'
        desc = '' if obj.doc.strip() == '' else '<p>%s</p>' % format_text(obj.doc)
        doc.append(tmp % { 'name': key, 'desc': desc })
        # Normal type details...

        if obj.is_enum:
            if obj.values:
                doc.append('<dl class="fields">\n<dt>Enumerated Values:</dt>')
                doc.extend( ['<dd><span class="pname">%s</span></dd>' % v for v in obj.values ])
                doc.append('</dl>')
                type_info = "enum"
        elif obj.is_struct:
            if obj.field_list:
                doc.append('<dl class="fields">\n<dt>Structure Field List:</dt>')
                tmp = '<dd><span class="pname">%s</span> : <span class="ptype">%s</span></dd>'
                doc.extend( [ tmp % (f.name, link_type(f.data_type.name)) for f in obj.field_list] )
                doc.append('</dl>')
                type_info = "struct"

        # Used-by details
        if len(obj['used_by']):
            users = obj['used_by'].keys()
            users.sort()
            ##print users
            doc.append('<dl class="usedby">\n<dt>Used by:</dt>\n<dd>')
            doc.extend(['<span>%s</span> ' % link_type(name) for name in users ])
            doc.append('</dd>\n</dl>')

        # Extra info section for developers
        if obj.is_class or obj.is_type or (obj.is_struct and obj.wraps_array):
            #convert to resource pointer
            if obj.is_pointer_wrapper:
                #if not isinstance(obj, SGCodeModule):
                type_info = 'is_pointer_wrapper: ' + obj.data_type.name + " : " + obj.data_type.related_type.name 
            elif obj.is_data_wrapper:
                type_info = 'is_data_wrapper: ' + obj.fields['data'].data_type.name
                if obj.fields['data'].data_type.related_type:
                    type_info = " : " +  obj.fields['data'].data_type.related_type.name
            elif obj.wraps_array:
                type_info = 'wraps_array: ' + obj.fields['data'].data_type.name
                if obj.fields['data'].data_type.related_type:
                    type_info = " : " +  obj.fields['data'].data_type.related_type.name 
            elif obj.data_type.is_procedure: 
                pass
            else:
                assert False
        else:
            pass
            #type_info = 'struct/enum'
        
        info = []
        info.append('<li><a href="%s" target="new">source code</a></li>' % source_url(obj.data_type.meta_comment_line_details))
        # tags / document group details?
        if obj.doc_group:
            info.append('<li>tags: %s</li>' % obj.doc_group) 
##        # library unique name for this method
##        info.append('<li>uname: <span class="code">%s</span></li>' % obj.uname )
        # is class/ via_pointer, info_type
        if obj.is_class: info.append('<li>is class</li>' )
        if obj.via_pointer: info.append('<li>via pointer</li>' )                    
        if obj.data_type.same_as: info.append('<li>same_as: %s</li>' % obj.data_type.same_as )                    
        # type info
        if type_info not in ['enum','struct']:
            info.append('<li>type: <span class="code">%s</span></li>' % type_info )
        # create info div 
        doc.append('<div class="info">\n<ul>\n%s\n</ul>\n</div>' % '\n'.join(info))
        
        # doc.append('''
        # <div class='info'>
        #     <ul>
        #     <li>uname: %(uname)s</li>
        #     <li>is_class: %(class)s</li>
        #     <li>via_pointer: %(via_pointer)s</li>
        #     <li>same_as: %(same_as)s</li>
        #     <li>type info: %(type_info)s</li>
        #     <li><a target="new" href="%(source_url)s">source code url</a></li>
        #     <li>doc_group: <a href="#group_%(doc_group)s">%(doc_group)s</a></li>
        #     </ul>
        # </div>
        # '''  % {
        #     'uname': obj.uname, 
        #     'class': obj.is_class,
        #     'via_pointer': obj.via_pointer,
        #     'same_as': obj.data_type.same_as,
        #     'type_info': type_info,
        #     'source_url': source_url(obj.data_type.meta_comment_line_details),
        #     'doc_group': obj.doc_group,
        # })            
        # Close section
        doc.append('</div>')
    
    doc.savetofile('Types.html')
    print 'Done.'



#==============================================================================
# MAIN
#==============================================================================
    
def main():
    # coppy
  
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    # Parse all files ready for use...
    print 'Parsing all pas units...'
    parser_runner.parse_all_units()
    # Extract method/type details and create identifiers/Types pages
    idc = IdentifierCollector()
    # Create all unit (module) pages
    UnitPresenter(idc)
    # Create the index.html page
    IndexPresenter(idc)
    # Create indentifiers.html and Types.html pages
    create_identifiers_doc(idc)
    create_types_doc(idc)

if __name__ == '__main__':
    main()
