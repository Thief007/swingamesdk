//----------------------------------------------------------------------------
// PointerManager.h
//----------------------------------------------------------------------------
//
//  Contains code used by the SwinGame resources. used by SGWrapperGen
//
//----------------------------------------------------------------------------

#import <Foundation/NSDictionary.h>
#import <Foundation/NSObject.h>

@interface PointerManager : NSObject

+ (void)initialize;
+ (void)registerObject:(id)obj withKey:(id)key;

+ (id)objectForKey:(id)key;
    
@end