#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>

#import "SG%(name)s.h"
#import "PointerManager.h"
#import "SGSDK.h"
#import "SwinGame.h"

@implementation SG%(name)s : NSObject

+ (id)createWithId:(id)ptr
{
    id obj = [PointerManager objectForKey: ptr];
    
    if (obj == nil)
    {
        // Create and assign to obj...
        obj = [[self alloc] initWithId:ptr];
        if (obj != nil)
        {
            // if this was not nil then register
            [PointerManager registerObject:obj withKey:ptr];
        }
    }
    
    return obj;
}

- (id)initWithId:(id)ptr
{
    //Assign super's initialised value to the self pointer
    self = [super init];
    if (self != nil)
    {
        //If self isn't nil then assign pointer.
        pointer = ptr;
    }
    return self;
}

- (void)releasePointer
{
    pointer = nil;
}

%(static_method_bodys)s

%(init_bodys)s

%(method_bodys)s

@end