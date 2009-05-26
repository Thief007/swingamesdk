#!/usr/bin/env python
# encoding: utf-8
'''Reads in a pascal source code file and outputs an abstract
syntax tree, with attached meta tags.

Meta tag syntax:
> /// @tag [parameters] 

parameters= parameter[, parameters]
parameter= <value>|([parameters])
pas_parser.py

Created by Andrew Cain on 2009-05-25.
Copyright (c) 2009 __MyCompanyName__. All rights reserved.
'''

from SGPasTokeniser import SGPasTokeniser

DEBUG = False




def main():
    tokeniser = SGPasTokeniser('../../CoreSDK/src/SGSDK_Audio.pas')
    tok = tokeniser.next_token()
    while tok[1] != 'implementation':
        print tok
        if tok[0] == 'attribute':
            tok = tokeniser.next_token()
            print tok
            if tok[1] == 'param':
                tok = tokeniser.next_token()
                print tok
                print tokeniser.read_to_eol()
            elif tok[1] == 'note':
                print tokeniser.read_to_eol()
        tok = tokeniser.next_token()


if __name__ == '__main__':
    main()

