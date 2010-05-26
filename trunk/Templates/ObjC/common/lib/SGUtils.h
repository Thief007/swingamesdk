#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>

#import "Types.h"


@interface SGUtils : NSObject

+ (NSArray *) arrayOfIntegers:(int *)firstPtr size:(int)sz;
+ (void) getIntegers:(int *)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz;

+ (NSArray *) arrayOfStrings:(char **)firstPtr size:(int)sz;
+ (NSArray *) arrayOfTriangles:(triangle *)firstPtr size:(int)sz;

@end