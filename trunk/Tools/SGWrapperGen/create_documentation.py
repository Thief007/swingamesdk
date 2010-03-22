#!/usr/bin/env python
# encoding: utf-8
"""
check_pas_parameters.py

Created by Andrew Cain on 2010-03-19.
Updates (HTML+CSS+TOC) by Clinton Woodward
Copyright (c) 2010 Swinburne University of Technology. All rights reserved.
"""

import logging
import sys

from sg import parser_runner
from sg.sg_cache import logger, find_or_add_file
from sg.sg_type import SGType
from sg.sg_parameter import SGParameter
from sg.file_writer import FileWriter

_out_path="../../Templates/Documentation"

_toc = []
_body = []

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

.sig, .pname, .ptype, .rtype { font-family: monospace; }
.pname, .pmod { font-weight: bold }
.pmod { color: blue }

h3 {padding: 0; margin: 0;}
dl { background: #FAFAFA }
dl dl { border: 0;}
dt {font-weight: bold}
dd dt {font-weight: normal }

#footer { background: #000; color: #FFF; }

#topnav { background: #fff; font-size: small}
</style>
</head>
<body>
<div id="toc">
    <ul>
%(toc)s
    </ul>
</div>
<p id="topnav"><a href="index.html">API Index</a></p>

<h1>%(title)s</h1>
<h2>Description</h2>
<p id="desc">%(desc)s</p>

<h2>Details</h2>

%(body)s

</body>
</html>
'''
#<div id="footer">
#Generated : %(datetime)s

def format_toc():
    '''convert toc list of tuples to list items'''
    tmp = []
    last = ''
    for title in _toc:
        if title != last:
            last = title
            tmp.append('        <li><a href="#%s">%s</a></li>' % (title, title))
    return '\n'.join(tmp)


def format_text(text):
    '''Convert text to valid xhtml+css markup'''
    #TODO: Replace this simple code detection with a robust technique
    #text = text.replace(' `','<pre>')
    #text = text.replace('` ','</pre>')
    return '<p>'+text+'</p>'


def method_visitor(method, other):
    '''Format the current method details and store in the global body and toc lists '''
    # Keep the toc entry (name and unique ID for hyperlinks)
    _toc.append(method.name )
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
        sig = '%s(%s) : %s' % (method.name, param_txt, method.return_type)
    else:
        sig = '%s(%s)' % (method.name, param_txt)
        
    # Print the headings    
    tmp = '<div class="method" id="%(id)s">\n<h3>%(name)s</h3>\n' + \
          '<p class="sig">%(sig)s</p>\n' + \
          '%(desc)s\n'
    _body.append(tmp % {'id': method.name, #TODO: broken - needs unique id's for overloads
                        'name': method.name, 
                        'desc': format_text(method.doc), 
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
                _body.append(tmp % {'pname': p.name, 'ptype': p.data_type.name, 'pdesc': p.doc })
            _body.append('</dl>\n</dd>')
        # RETURN TYPE DETAILS    
        if method.return_type:
            tmp = '<dt>Returns:</dt>\n' + \
                  '<dd><span class="rtype">%(rtype)s</span> : %(rdesc)s</dd>'
            _body.append(tmp % {'rtype': method.return_type.name, 'rdesc': method.return_type.doc })
        # END LIST
        _body.append('</dl>')

    tmp = '''
    <p><strong>See also</strong>: </p>
    <p><strong>Deprecated</strong>: </p>
    <p><strong>See also</strong>: </p>
    '''
    
    _body.append("\n</div>\n")


def visitor(the_file, other):
    # Don't do some files...
    if the_file.name in ['SGSDK', 'Types']: return
    # Clear the current body and toc contents
    _body[:] = []
    _toc[:] = []
    
    # Here we go...
    print '>> %s ... ' % (the_file.name),
    # All units have only one code-module per file, and it's in the first position.
    the_file.members[0].visit_methods(method_visitor, None)
    # Create output file
    file_writer = FileWriter('%s/%s.html'% (_out_path, the_file.name))
    # Extract content details: toc, body etc
    tmp = {
        'title': the_file.name, 
        'desc': the_file.members[0].doc, 
        'toc': format_toc(),
        'body': "\n".join(_body),
        'datetime': '--NOW--',
    }
    file_writer.write(html % tmp)    
    # We're done 
    print 'Done!'

def main():
    logging.basicConfig(level=logging.WARNING,format='%(asctime)s - %(levelname)s - %(message)s',stream=sys.stdout)
    parser_runner.run_for_all_units(visitor)

if __name__ == '__main__':
    main()
