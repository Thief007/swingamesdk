#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>

#import "Types.h"
#import "SGTypes.h"
%(imports)s

@interface SG%(name)s : NSObject
{
@package
    %(name)s pointer;
}

+ (NSArray *) arrayOf%(name)ss:(%(name)s *)firstPtr size:(int)sz;
+ (void) get%(name)ss:(%(name)s *)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz;
+ (id)createWithId:(%(name)s)ptr;
%(static_method_headers)s
- (id)initWithId:(%(name)s)ptr; 
%(init_headers)s
%(method_headers)s
@end