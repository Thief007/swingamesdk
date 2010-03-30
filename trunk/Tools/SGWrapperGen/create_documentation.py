#!/usr/bin/env python
# encoding: utf-8
"""
check_pas_parameters.py

Created by Andrew Cain on 2010-03-19.
Updates (HTML+CSS+TOC) by Clinton Woodward
Copyright (c) 2010 Swinburne University of Technology. All rights reserved.
"""
# TODO's
# Line numbers - link to svn view of files (need revision for url)
# http://swingamesdk.googlecode.com/svn/trunk/CoreSDK/src/sgAnimations.pas
# http://code.google.com/p/swingamesdk/source/browse/trunk/CoreSDK/src/sgAnimations.pas#44


import logging, sys, re, time

from sg import parser_runner
from sg.sg_cache import logger, find_or_add_file
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter
from sg.file_writer import FileWriter

_out_path="../../Templates/Documentation"

_files = []
_toc = []
_body = []
_indentifiers = {}

html = '''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" 
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<title>%(title)s</title>
<style type="text/css">

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

.method {border-top: 1px solid #55F; padding: 1em; }
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

#footer { background: #eee; color: #500;font-size: 0.8em}

#topnav { background: #eee; font-size: 0.8em; padding: 0; }
#topnav ul, #topnav li {list-style-type: none; margin: 0; padding: 0 }
#topnav li {display: inline; margin: 0 2em; }
#topnav a { color: #500; }

.info {color: #005; background: #eee; font-size: 0.8em }

</style>
</head>
<body>
<div id="topnav">
<ul>
<li><a href="index.html">API Index</a></li>
<li><a href="identifier.html">Identifiers</a></li>
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
Generated : %(datetime)s
</div>

</body>
</html>
'''

def format_toc():
    '''convert toc list of tuples (name, unmae) to list items. 
    Only 1 unique name is presented in the toc for brevity.
    '''
    tmp = []
    last = ''
    for title, uname in _toc:
        if title != last:
            last = title
            tmp.append('        <li><a href="#%s" title="%s">%s</a></li>' % (uname, uname, title))
    return '\n'.join(tmp)


p_code = re.compile(r'``(.*?)``')
p_kind = re.compile(r'`(.*?)`')

_nolink_types = ('Single','String', 'Boolean', 'LongInt')

def link_type(text):
    # convert text to hyperlinked version is type is known or not general
    text = text.strip()
    if text not in _nolink_types:
        text = '<a class="code" href="identifer.html#%s">%s</a>' % (text, text)
    return text
    
def format_text(text):
    '''Convert text to valid xhtml+css markup'''
    # Convert double-ticks ``-`` into code formatting (no hypertext link)
    text = p_code.sub(r'<span class="code">\1</span>' , text)
    # Convert single-ticks `-` into kind (identifier) links
    #TODO: need test of "method" identifier (module.method) or data "type" identifier 
    # ie each match need to be tested for kind of match (dict lookup) callback
    text = p_kind.sub(r'<a class="code" href="identifer.html#\1">\1</a>' , text)
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
    _body.append(tmp % {'uname': method.uname, # unique id's for overloads
                        'name': method.name, 
                        'desc': '<p>%s</p>' % format_text(method.doc), 
                        'sig': sig })
    
    # If parameters and/or return type details
    if len(method.params) > 0 or method.return_type:
        # START LIST    
        _body.append('<dl class="fields">')
        # PARAMETERS
        if len(method.params) > 0:
            _body.append('<dt>Parameters:</dt>\n<dd>\n<dl>')
            for p in method.params:
                tmp = '<dt><span class="pname">%(pname)s</span> : <span class="ptype">%(ptype)s</span></dt>' + \
                      '<dd>%(pdesc)s</dd>'
                _body.append(tmp % {'pname': p.name, 
                                    'ptype': link_type(p.data_type.name), 
                                    'pdesc': format_text(lead_trim(p.doc)) })
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
    tmp = '''
    <p><strong>Side Effects</strong>: </p>
    <p><strong>See also</strong>: </p>
    <p><strong>Deprecated</strong>: </p>
    <p><strong>See also</strong>: </p>
    '''
    
    tmp = '''
    <div class='info'>
    <ul>
    <li>lib uname: %(uname)s</li>
    <li>in_class: %(in_class)s</li>
    <li>method_called: %(method_called)s</li>
    </ul>
    </div>
    '''
    _body.append(tmp % {'uname': method.uname, 
                        'in_class': method.in_class,
                        'method_called': method.method_called} )
    
    _body.append("\n</div>\n")


#def 

def visitor(the_file, other):
    # Don't do some files...
    #if the_file.name in ['SGSDK', 'Types']: return
    if the_file.name in ['SGSDK']: return
    # Keep the filename for the index 
    _files.append(the_file.name)
    # Clear the current body and toc contents
    _body[:] = []
    _toc[:] = []
    
    # Here we go...
    print '>> %s ... ' % (the_file.name),
    # All units have only one code-module per file, and it's in the first position.
#    the_file.members[0].visit_methods(method_visitor, None)
    # 
    for m in the_file.members:
        m.visit_methods(method_visitor, None)
    # Create output file
    file_writer = FileWriter('%s/%s.html'% (_out_path, the_file.name))
    # Extract content details: toc, body etc
    tmp = {
        'title': the_file.name, 
        'desc': format_text(the_file.members[0].doc), 
        'toc': format_toc(),
        'body': "\n".join(_body),
        'datetime': time.strftime('%Y-%m-%d %H:%M:%S'), 
    }
    file_writer.write(html % tmp) 
    file_writer.close()   
    # We're done 
    print 'Done!'



index_html = '''<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" 
   "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<title>SwinGame API</title>
<style type="text/css">
body { 
  font-family: Arial; 
  background: #FFF url(low_cog.gif) no-repeat;
  background-attachment: fixed;
  background-position: right bottom;
}
</style>
</head>
<body>

<h1>SwinGame API Documentation</h1>

<ul>
 %(links)s
</ul>

</body>
</html>
'''


def create_index_page():
    print "Creating index.html ...",
    # Build up the list of files as links 
    links = []
    for name in _files:
        links.append("<li><a href='%s.html'>%s</a>" % (name, name))
    links = '\n'.join(links)
    # Create the file, merge links with the template
    file_writer = FileWriter('%s/%s.html'% (_out_path, "index"))
    file_writer.write(index_html % {'links': links })
    file_writer.close()
    print 'Done.'
    
def create_indentifer_page():
    pass
    
def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    parser_runner.run_for_all_units(visitor)
    #Create the index.html page
    create_index_page()
    #TODO: Create the identifer.html page

if __name__ == '__main__':
    main()
