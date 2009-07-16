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

+ (NSArray *) arrayOf%(name)ss:(%(name)s *)firstPtr size:(int)sz;
+ (SG%(name)s *) %(camel_name)sForData: (%(name)s)dat;
+ (void) get%(name)ss:(%(name)s *)firstPtr fromArray:(const NSArray *)arr maxSize:(int)sz;

- (SG%(name)s *)initWith%(name)s:(%(name)s)dat;

- (%(name)s) data;
- (void) setData:(%(name)s)dat;

%(static_method_headers)s

%(init_headers)s

%(method_headers)s

@end