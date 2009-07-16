#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>

#import "SG%(name)s.h"
#import "PointerManager.h"
#import "SGSDK.h"

@implementation SG%(name)s : NSObject

+ (SG%(name)s *) %(camel_name)sForData: (%(name)s)dat;
{
    SG%(name)s *ret = [[SG%(name)s alloc] initWith%(name)s: dat];
    [ret autorelease];
    return ret;
}

- (id)initWith%(name)s:(%(name)s)dat;
{
    //Assign super's initialised value to the self pointer
    self = [super init];
    if (self != nil)
    {
        //If self isn't nil then assign pointer.
        data = dat;
    }
    return self;
}

- (%(element.type)s) valueAtIndex:%(element.idx.params)s
{
    return data[%(element.idx.expr)s];
}

- (void) setValueAtIndex:%(element.idx.params)s toValue:(%(element.type)s)dat
{
    data[%(element.idx.expr)s] = dat;
}

%(static_method_bodys)s

%(init_bodys)s

%(method_bodys)s

@end
