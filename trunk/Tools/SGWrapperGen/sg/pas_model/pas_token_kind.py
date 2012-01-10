class TokenKind:
    """
    Enumeration-type object that stores the different kinds of tokens that are
    available for use.

    number,         # such as 1234, 123.45, -123, +123.4
    comment,        # single // or multi-line (* ... *), { ... } 
    meta comment,   # start with /// ...
    identifier      # identifier name starting with alpha, including 
                    # alpha-numeric characters and the _ character
    attribute,      # name starting with @... inside meta comment block 
                    # follows the id name character rules
    operator,       # one of + - / * ** := < > <= >= <>
    symbol,         # one of ':;,.()'
    boolean,        # either true or false

    """
    Identifier = 'Identifier'
    Number = 'Number'
    Comment = 'Comment'
    MetaComment = 'Meta Comment'
    Attribute = 'Attribute'
    Operator = 'Operator'
    Symbol = 'Symbol'
    Boolean = 'Boolean'



