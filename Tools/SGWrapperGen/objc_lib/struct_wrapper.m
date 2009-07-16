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

+ (void) get%(name)ss:(%(name)s *)firstPtr fromArray:(const NSArray *)arr maxSize:(int)sz
{
    int i, count = [arr count];
    count = count <= sz ? count: sz; //get min of count and sz
    
    for (i = 0; i < count; i++)
    {
        *(firstPtr + i) = [((SG%(name)s *)[arr objectAtIndex: i]) data];
    }
}

- (SG%(name)s *)initWith%(name)s:(%(name)s)dat;
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

- (%(name)s) data;
{
    return data;
}

- (void) setData:(%(name)s)dat;
{
    data = dat;
}

%(static_method_bodys)s

%(init_bodys)s

%(method_bodys)s

@end
