//----------------------------------------------------------------------------
// PointerWrapper.m
//----------------------------------------------------------------------------
//
//  Contains code used by the SwinGame resources. used by SGWrapperGen
//
//----------------------------------------------------------------------------

// The ptrRegistry is responsible for maintaining copies of all wrapped SwinGame pointers.
#import "PointerManager.h"
#import "SGSDK.h"

static NSMutableDictionary *_ptrRegister;

void removeObject(void *ptr)
{
    id obj = [_ptrRegister objectForKey:ptr];
    if (obj != nil)
    {
        [obj releasePointer];
        [_ptrRegister removeObjectForKey: ptr];
    }
}

@implementation PointerManager : NSObject

+ (void)initialize
{
    _ptrRegister = [[NSMutableDictionary alloc] initWithCapacity: 1000];
    sg_Resources_RegisterFreeNotifier(removeObject);
}

+ (void)registerObject:(id)obj withKey:(id)key
{
    [_ptrRegister setObject:obj forKey:key];
}

+ (id)objectForKey:(id)key
{
    return [_ptrRegister objectForKey:key];
}

@end