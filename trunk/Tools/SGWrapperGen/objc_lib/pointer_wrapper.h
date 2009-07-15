#import <Foundation/NSObject.h>
#import <Foundation/NSArray.h>

#import "Types.h"
#import "SGTypes.h"
%(imports)s

@interface SG%(name)s : NSObject
{
@package
    id pointer;
}

+ (id)createWithId:(id)ptr;
- (id)initWithId:(id)ptr;

%(static_method_headers)s

%(init_headers)s

%(method_headers)s

@end