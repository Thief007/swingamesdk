#import "SGUtils.h"

#import <Foundation/NSValue.h>

@implementation SGUtils : NSObject

+ (NSArray *) arrayOfIntegers:(int *)firstPtr size:(int)sz
{
    NSMutableArray *result = [[NSMutableArray alloc] initWithCapacity:sz];
    int i;
    NSNumber *obj;
    
    for (i = 0; i < sz; i++)
    {
        obj = [NSNumber numberWithInt: *(firstPtr + i)]; //obj is autorelease...
        [result addObject: obj];
    }
    
    return [result autorelease];
}

+ (void) getIntegers:(int *)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz
{
    int i;
    int count = [in_data count];
    count = count <= sz ? count : sz; //get min of sz and count
    for ( i = 0; i < count; i++ ) 
    {
        NSNumber *num = [in_data objectAtIndex:i];
        *(firstPtr + i) = [num intValue];
    }
}

+ (NSArray *) arrayOfStrings:(char **)firstPtr size:(int)sz
{
    NSMutableArray *result = [[NSMutableArray alloc] initWithCapacity:sz];
    int i;
    NSString *obj;
    
    for (i = 0; i < sz; i++)
    {
        obj = [[[NSString alloc] initWithCString:*(firstPtr + i encoding:NSASCIIStringEncoding)] autorelease]; //obj is autorelease...
        [result addObject: obj];
    }
    
    return [result autorelease];
}

@end