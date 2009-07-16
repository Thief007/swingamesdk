#import "SG%(name)s.h"

#import "PointerManager.h"
#import "SGSDK.h"
#import "SwinGame.h"

@implementation SG%(name)s : NSObject

+ (void) get%(name)ss:(%(name)s *)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz
{
    int i;
    int count = [in_data count];
    count = count <= sz ? count : sz; //get min of sz and count
    for ( i = 0; i < count; i++ ) 
    {
        SG%(name)s *obj = [in_data objectAtIndex:i];
        *(firstPtr + i) = obj->pointer;
    }
}

+ (NSArray *) arrayOf%(name)ss:(%(name)s *)firstPtr size:(int)sz
{
    NSMutableArray *result = [[NSMutableArray alloc] initWithCapacity:sz];
    int i;
    SG%(name)s *obj;
    
    for (i = 0; i < sz; i++)
    {
        obj = [SG%(name)s createWithId: *(firstPtr + i)];
        [result addObject: obj];
    }
    
    return [result autorelease];
}

+ (id)createWithId:(%(name)s)ptr
{
    id obj = [PointerManager objectForKey: (id)ptr];
    
    if (obj == nil)
    {
        // Create and assign to obj...
        obj = [[self alloc] initWithId:ptr];
    }
    
    return obj;
}

- (id)initWithId:(%(name)s)ptr
{
    //Assign super's initialised value to the self pointer
    self = [super init];
    if (self != nil)
    {
        //If self isn't nil then assign pointer.
        pointer = ptr;
        [PointerManager registerObject:self withKey:(id)ptr];
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