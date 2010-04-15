#!/usr/bin/env python
# encoding: utf-8
"""
check_pas_parameters.py

Created by Andrew Cain on 2010-03-19.
Updates (HTML+CSS+TOC) by Clinton Woodward
Copyright (c) 2010 Swinburne University of Technology. All rights reserved.
"""

import logging, sys, re, time, subprocess

from sg import parser_runner
from sg.sg_cache import logger, find_or_add_file
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter
from sg.file_writer import FileWriter

#==============================================================================
# Settings and global data ...
#==============================================================================

def get_svn_version():
    try:
        lines = subprocess.Popen("svn info",shell=True, stdout=subprocess.PIPE).stdout.readlines()
        print "svn info: ", lines[4]
        result = lines[4].split()[1].strip() # "Revision: 12345"
    except:
        result = "1250"
    return result

_out_path = "../../Templates/Documentation"

_svn_version = get_svn_version()
_google_base_url = "http://code.google.com/p/swingamesdk/source/browse/trunk/CoreSDK/src/"

_files = []
_toc = []
_body = []
_indentifiers = {}

_nolink_types = ('Single', 'String', 'Boolean', 'LongInt', 'Byte')


_common_css = '''

body, th { font-family: Arial; }
td, th { border: 1px solid #888}
table {border-collapse: collapse;}
th {padding: 0 0.5em; }
.none {background-color: #DDD; }
tr:hover td, tr:hover td { background-color: #FFD}
.problem td {padding: 0 0.5em }
a img {border: none}
.note { font-weight: bold; color: blue}
.min { font-weight: bold; color: red}
.max { font-weight: bold; color: red}
.left { text-align: left; }

body {
  background: #FFF url(low_cog.gif) no-repeat;
  background-attachment: fixed;
  background-position: right bottom;
}

#toc, #toc ul, #toc li {border: none; padding: 0; margin: 0; }
#toc { background: #fafafa; width: 10em; float: right; border: None}
#toc ul, #toc li { list-style-type: none; font-size: 8pt;}
#toc li li {display:block; padding-left: 1em}
#toc a {
  border-bottom: 1px solid #555;
  border-left: 3px solid #555; 
  text-decoration: none; background: white; 
  display: block; padding: 1px;
  color: #000; 
}
#toc a:hover {background: #eef;}

.method, .type {border-top: 1px solid #55F; padding: 1em; }
.sig {background: #F5F5F5; display: inline}

.sig, .pname, .ptype, .rtype, .code { font-family: monospace; }
.pname, .pmod { font-weight: bold }
.pmod { color: #505 }

a.code { text-decoration: none; }

h3 {padding: 0; margin: 0;}
dl { background: #FAFAFA }
dl dl { border: 0;}
dt {font-weight: bold}
dd dt {font-weight: normal }

#footer { background: #eee; color: #500;font-size: 0.8em; clear: both}

#topnav { background: #eee; font-size: 0.8em; padding: 0; }
#topnav ul, #topnav li {list-style-type: none; margin: 0; padding: 0 }
#topnav li {display: inline; margin: 0 2em; }
#topnav a { color: #500; }
#topnav .current {text-decoration: none; font-weight: bold; }

.info {color: #005; background: #eee; font-size: 0.8em }
'''

#==============================================================================
# Unit content page unitname.html template ...
#==============================================================================
_unit_html = '''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" 
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<title>%(title)s</title>
<style type="text/css">''' + _common_css + '''</style>
</head>
<body>
<div id="topnav">
<ul>
<li><a href="index.html">API Index</a></li>
<li><a href="identifiers.html">Identifiers</a></li>
<li><a href="Types.html">Types</a></li>
</ul>
</div>
<div id="toc">
    <ul>
%(toc)s
    </ul>
</div>

<h1>%(title)s</h1>
<h2>Description</h2>
<p id="desc">%(desc)s</p>

<h2>Details</h2>

%(body)s

<div id="footer">
Generated %(datetime)s for svn version %(svnversion)s
</div>

</body>
</html>
'''

#==============================================================================
# Index index.html page ...
#==============================================================================
_index_html = '''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" 
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<title>SwinGame API</title>
<style type="text/css">''' + _common_css + '''</style>
</head>
<body>
<div id="topnav">
<ul>
<li><a href="index.html" class="current">API Index</a></li>
<li><a href="identifiers.html">Identifiers</a></li>
<li><a href="Types.html">Types</a></li>
</ul>
</div>
<h1>SwinGame API Documentation</h1>

<dl>
    <dt><a href="identifiers.html">Identifiers</a></dt>
    <dd>An alphbetical list of all method names and type identifiers. 
    A text "search" in this page for key words might help you find just what 
    you need...</dd>
    
    <dt><a href="Types.html">Types</a></dt>
    <dd>Data type details so you know who contains what!</dd>
</dl>

<h2>Modules</h2>
<dl>
 %(links)s
</dl>

<div id="footer">
Generated %(datetime)s for svn version %(svnversion)s
</div>
</body>
</html>
'''


#==============================================================================
# Functions ....
#==============================================================================

def format_toc(toc):
    '''convert toc list of tuples (name, unmae) to list items. 
    Only 1 unique name is presented in the toc for brevity.
    '''
    tmp = []
    last = ''
    for title, uname in toc:
        if title != last:
            last = title
            tmp.append('        <li><a href="#%s" title="%s">%s</a></li>' % (uname, uname, title))
    return '\n'.join(tmp)

# Regular expressions for catching `linked-code` and ``code-formated`` text
p_code = re.compile(r'``(.*?)``')
p_kind = re.compile(r'`(.*?)`')

def link_type(text):
    # convert text to hyperlinked version is type is known or not general
    #TODO: link to either METHODS or TYPES not the identifiers page...
    text = text.strip()
    # hack
    if '[' in text: return text 
    #
    if text not in _nolink_types:
        text = '<a class="code" href="identifiers.html#%s">%s</a>' % (text, text)
    return text
    
def format_text(text):
    '''Convert text to valid xhtml+css markup'''
    # Convert double-ticks ``-`` into code formatting (no hypertext link)
    text = p_code.sub(r'<span class="code">\1</span>' , text)
    # Convert single-ticks `-` into kind (identifier) links
    #TODO: need test of "method" identifier (module.method) or data "type" identifier 
    # ie each match need to be tested for kind of match (dict lookup) callback
    text = p_kind.sub(r'<a class="code" href="identifiers.html#\1">\1</a>' , text)
    # reformat paragraph breaks for pretty presentation
    if len(text.strip()) > 0:
        lines= text.split('\n')
        for i, line in enumerate(lines):
            if line.strip() == '': lines[i] = '</p><p>' 
        text = '\n'.join(lines)
    # done
    return text
    
def lead_trim(text):
    '''Remove leading "-" or ":" from text as it sometimes appears in parameter 
    description text.'''
    text = text.strip()
    if len(text) > 0 and text[0] in ['-',':']:
        text = text[1:].strip()
    return text


def method_visitor(method, other):
    '''Format the current method details and store in the global body and toc lists '''
    # Keep the toc entry (name and unique ID for hyperlinks)
    _toc.append( (method.name, method.uname) )
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
    _body.append(tmp % {'uname': method.uname, # unique id's for overloads
                        'name': method.name, 
                        'desc': desc, 
                        'sig': sig })
    
    # If parameters and/or return type details
    if len(method.params) > 0 or method.return_type:
        # START LIST    
        _body.append('<dl class="fields">')
        # PARAMETERS
        if len(method.params) > 0:
            _body.append('<dt>Parameters:</dt>\n<dd>\n<dl>')
            for p in method.params:
                tmp = '<dt><span class="pname">%(pname)s</span> : <span class="ptype">%(ptype)s</span></dt>\n%(pdesc)s'
                if len(lead_trim(p.doc).strip()) > 0:
                    pdesc = '<dd>%s</dd>' % format_text(lead_trim(p.doc))
                else:
                    pdesc = ''
                _body.append(tmp % {'pname': p.name, 
                                    'ptype': link_type(p.data_type.name), 
                                    'pdesc': pdesc })
            _body.append('</dl>\n</dd>')
        # RETURN TYPE DETAILS    
        if method.return_type:
            tmp = '<dt>Returns:</dt>\n' + \
                  '<dd><span class="rtype">%(rtype)s</span> : %(rdesc)s</dd>'
            _body.append(tmp % {'rtype': link_type(method.return_type.name), 
                                'rdesc': format_text(lead_trim(method.returns)) })
        # END LIST
        _body.append('</dl>')

    #TODO: not used yet
    # tmp = '''
    # <p><strong>Side Effects</strong>: </p>
    # <p><strong>See also</strong>: </p>
    # <p><strong>Deprecated</strong>: </p>
    # <p><strong>See also</strong>: </p>
    #'''
    
    # Meta-details 
    tmp = '''
    <div class='info'>
    <ul>
    <li>uname: %(uname)s</li>
    <li>in_class: %(in_class)s</li>
    <li>method_called: %(method_called)s</li>
    <li><a target="new" href="%(source_url)s">source code url</a></li>
    <li>doc_group: <a href="#group_%(doc_group)s">%(doc_group)s</a></li>
    </ul>
    </div>
    '''
    # create code.google.com link
    bits = method.file_line_details.split()
    line_no = bits[3]
    fname = bits[5].split('/')[-1].strip()
    source_url = _google_base_url + fname + "?r=" + _svn_version + '#' + line_no
    #
    _body.append(tmp % {'uname': method.uname, 
                        'in_class': method.in_class,
                        'method_called': method.method_called,
                        'source_url': source_url,
                        'doc_group': method.doc_group} )
    
    #TODO: Keep track of doc groups, list at end of file with back-links.
    
    _body.append("\n</div>\n")

def type_visitor(method, other):
    # '''Write out a single c member'''    
    # assert member.is_class or member.is_struct or member.is_enum or member.is_type
    # 
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
    pass

def visitor(the_file, other):
    # Don't do some files...
    if the_file.name in ['SGSDK', 'Types']: return
    #if the_file.name in ['SGSDK']: return
    # Keep the filename for the index 
    _files.append( (the_file.name, format_text(the_file.members[0].doc)) )
    # Clear the current body and toc contents
    _body[:] = []
    _toc[:] = []
    
    # Here we go...
    print '>> %s ... ' % (the_file.name),
    for m in the_file.members:
        if m.is_module:
            # procedures / functions
            m.visit_methods(method_visitor, None)
        elif m.is_class or m.is_struct or m.is_enum or m.is_type:
            # class/struct/enum/type stuff
            type_visitor(m, None)
            
    # Create output file
    file_writer = FileWriter('%s/%s.html'% (_out_path, the_file.name))
    # Extract content details: toc, body etc
    tmp = {
        'title': the_file.name, 
        'desc': format_text(the_file.members[0].doc), 
        'toc': format_toc(_toc),
        'body': "\n".join(_body),
        'datetime': time.strftime('%Y-%m-%d %H:%M:%S'),
        'svnversion': _svn_version, 
    }
    file_writer.write(_unit_html % tmp) 
    file_writer.close()   
    # We're done 
    print 'Done!'



def create_index_page():
    print "Creating index.html ...",
    # Build up the list of files as links 
    links = []
    for name, desc in _files:
        links.append("<dt><a href='%s.html'>%s</a></dt>" % (name, name))
        links.append("<dd>%s</dd>" % desc)
    links = '\n'.join(links)
    # Create the file, merge links with the template
    file_writer = FileWriter( _out_path + '/index.html')
    tmp = {
        'links': links, 
        'datetime': time.strftime('%Y-%m-%d %H:%M:%S'),
        'svnversion': _svn_version, 
    }
    file_writer.write(_index_html % tmp)
    file_writer.close()
    print 'Done.'


#==============================================================================
# Indentifier indentifier.html page template ...
#==============================================================================

class IdentifierCollector(object):

    _identifier_html = '''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" 
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<title>SwinGame API Identifiers</title>
<style type="text/css">''' + _common_css + '''

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

</style>
</head>
<body>
<div id="topnav">
<ul>
<li><a href="index.html">API Index</a></li>
<li><a href="identifiers.html" class="current">Identifiers</a></li>
<li><a href="Types.html">Types</a></li>
</ul>
</div>

<h1>SwinGame API Identifiers</h1>

<h2>Methods</h2>
%(links)s
%(methods)s

<h2>Types</h2>
%(types)s

<div id="footer">
Generated %(datetime)s for svn version %(svnversion)s
</div>
</body>
</html>
''' 
   
    def __init__(self):
            
        ids = self.ids = {
            'methods': {},
            'umethods': {},
            'types': {},
            'structs': {},
            'enums': {},
            'classes': {},
        }

        ## 'Gathering identifier details:'
        parser_runner.visit_all_units(self._file_visitor)

        # build link-calls back to method parameters
        for key, method in ids['umethods'].items():
            if method.params:
                for p in method.params:
                    # only keep the special types, not LongInt etc
                    if p.data_type.name in ids['types']:
                        ids['types'][p.data_type.name]['used_by'][method.uname] = method


        # Create the identifiers.html and types.html docs
        self._create_identifiers_doc()
        self._create_types_doc()
            
    def _create_identifiers_doc(self):
        # Create the identifiers.html document body content ...
        tmp = []
        body = {}
        links = []
        ids = self.ids

        ## Methods 
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
        body['methods'] = '\n'.join(tmp)
        # Build a pretty alphabet of links for the top ...
        tmp = []
        for c in 'ABCDEFGHIJKLMNOPQRSTUVWXYZ':
            if c in links:
                tmp.append('<a href="#%s">%s</a>' % (c,c))
            else:
                tmp.append(c)
        body['links'] = '<ul id="links">\n<li>' + ('</li><li>'.join(tmp)) + '</li></ul>'

        ## Special Types:'
        tmp = []
        tmp.append('<ul id="types">')
        keys = ids['types'].keys()
        keys.sort()
        for key in keys:
            num = len(ids['types'][key]['used_by'])
            url = ids['types'][key]['doc_url']
            if num > 0:
                tmp.append('<li><a href="%s">%s</a> (%d)</li>' % (url, key, num))
            else:
                tmp.append('<li>%s (%d)</li>' % (key, num))
        tmp.append('</ul>')
        body['types'] = '\n'.join(tmp)
        
        body['datetime'] = time.strftime('%Y-%m-%d %H:%M:%S')
        body['svnversion'] = _svn_version
        
        # Create the single identifier file with all method names and special data types
        print 'Creating identifiers.html file...'
        file_writer = FileWriter(_out_path + '/identifiers.html')
        file_writer.write(self._identifier_html % body )
        file_writer.close()
        print 'Done.'
    
    def _create_types_doc(self):
        # Create Types.html with all the details we need
        ids = self.ids
                
        _body = []
        
        #print ids['types']
        toc = []
        
        keys = ids['types'].keys()
        keys.sort()
        for key in keys:
            obj = ids['types'][key]
            # keep a TOC entry
            toc.append((key, key))
            # Print the headings    
            tmp = '<div class="type" id="%(name)s">\n<h3>%(name)s</h3>\n%(desc)s\n'
            desc = '' if obj.doc.strip() == '' else '<p>%s</p>' % format_text(obj.doc)
            _body.append(tmp % { 'name': key, 'desc': desc })
            # Normal type details...

            if obj.is_enum:
                if obj.values:
                    _body.append('<dl class="fields"><dt>Enumerated Values:</dt>')
                    _body.extend( ['<dd><span class="pname">%s</span></dd>' % v for v in obj.values ])
                    _body.append('</dl>')
            elif obj.is_struct:
                if obj.field_list:
                    _body.append('<dl class="fields"><dt>Structure Field List:</dt>')
                    tmp = '<dd><span class="pname">%s</span> : <span class="ptype">%s</span></dd>'
                    _body.extend( [ tmp % (f.name, link_type(f.data_type.name)) for f in obj.field_list] )
                    _body.append('</dl>')

            # Used-by details
            if len(obj['used_by']):
                users = obj['used_by'].keys()
                users.sort()
                ##print users
                _body.append('<dl><dt>Used by:</dt>\n<dd>')
                _body.extend(['<span>%s</span> ' % link_type(name) for name in users ])
                _body.append('</dd>\n</dl>')

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
                type_info = 'struct/enum'
            
            tmp = '''
            <div class='info'>
            <ul>
            <li>uname: %(uname)s</li>
            <li>is_class: %(class)s</li>
            <li>via_pointer: %(via_pointer)s</li>
            <li>same_as: %(same_as)s</li>
            <li>type info: %(type_info)s</li>
            <li><a target="new" href="%(source_url)s">source code url</a></li>
            <li>doc_group: <a href="#group_%(doc_group)s">%(doc_group)s</a></li>
            </ul>
            </div>
            '''
            # create code.google.com link
            # TODO: need file_line_details set for types also...
            #bits = obj.file_line_details.split()
            line_no = '0' # bits[3]
            fname = 'sgTypes.pas' # bits[5].split('/')[-1].strip()
            source_url = _google_base_url + fname + "?r=" + _svn_version + '#' + line_no
            #
            _body.append(tmp % {'uname': obj.uname, 
                                'class': obj.is_class,
                                'via_pointer': obj.via_pointer,
                                'same_as': obj.data_type.same_as,
                                'type_info': type_info,
                                'source_url': 'TODO',
                                'doc_group': obj.doc_group} )            
            # Close section
            _body.append('</div>')
        
        toc.sort()
        # Create the single types files with all the types 
        print 'Creating Types.html file...'
        file_writer = FileWriter(_out_path + '/Types.html')
        tmp = {
            'title': 'Types', 
            'desc': format_text('DESCRIPTION'), 
            'toc': format_toc(toc),
            'body': "\n".join(_body),
            'datetime': time.strftime('%Y-%m-%d %H:%M:%S'), 
            'svnversion': _svn_version,            
        }
        file_writer.write(_unit_html % tmp)        
        file_writer.close()
        print 'Done.'        

    def _file_visitor(self, the_file, other):
        if the_file.name in ['SGSDK']: return
        print '>> %s ... ' % (the_file.name),
        for m in the_file.members:
            if m.is_module:
                # procedures / functions
                m.visit_methods(self._method_visitor, None)
            elif m.is_class or m.is_struct or m.is_enum or m.is_type:
                # class/struct/enum/type stuff
                self._type_visitor(m, None)
        print 'Done!'    

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
        else: 
            print 'i dunno...', member.name
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
      
    
    
def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    # Parse all files...
    parser_runner.parse_all_units()
    # Extract all the details needed identifier
    IdentifierCollector()
    # Parse all files ...
    parser_runner.visit_all_units(visitor)
    # #Create the index.html page
    create_index_page()

if __name__ == '__main__':
    main()
