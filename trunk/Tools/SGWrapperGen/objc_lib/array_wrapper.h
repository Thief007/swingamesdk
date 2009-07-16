#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>

#import "Types.h"
#import "SGTypes.h"

@interface SG%(name)s : NSObject
{
@package
    %(name)s data;
}

+ (SG%(name)s *) %(camel_name)sForData: (%(name)s)dat;

- (id)initWith%(name)s:(%(name)s)dat;

- (%(element.type)s) valueAtIndex:%(element.idx.params)s;
- (void) setValueAtIndex:%(element.idx.params)s toValue:(%(element.type)s)value;

%(static_method_headers)s

%(init_headers)s

%(method_headers)s

@end