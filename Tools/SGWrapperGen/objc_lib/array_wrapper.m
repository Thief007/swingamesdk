#import <string.h>
#import "SG%(name)s.h"

#import "SGSDK.h"
#import "SwinGame.h"

@implementation SG%(name)s : NSObject

+ (SG%(name)s *) %(camel_name)sForData: (%(name)s)dat
{
    SG%(name)s *ret = [[SG%(name)s alloc] initWith%(name)s: dat];
    [ret autorelease];
    return ret;
}

- (id)initWith%(name)s:(%(name)s)dat
{
    //Assign super's initialised value to the self pointer
    self = [super init];
    if (self != nil)
    {
        //If self isn't nil then assign pointer.
        memcpy((void *) data, (void *) dat, sizeof(data));
    }
    return self;
}

- (%(element.type)s) valueAtIndex:%(element.idx.params)s
{
    return %(element.access)s;
}

- (void) setValueAtIndex:%(element.idx.params)s toValue:(%(element.type)s)value
{
    data[%(element.idx.expr)s] = %(element.value)s;
}

%(static_method_bodys)s

%(init_bodys)s

%(method_bodys)s

@end
