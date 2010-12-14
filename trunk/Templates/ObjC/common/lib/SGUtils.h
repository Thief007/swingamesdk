#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>

#import "Types.h"
#import "SGTypes.h"

#import "SGTriangle.h"

@interface NSString (SGStringUtils)

+ (void) getStrings:(char **)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz;
+ (NSArray *) arrayOfStrings:(char **)firstPtr size:(int)sz;

@end

@interface SGTriangle (SGTriangleUtils)

+ (NSArray *) arrayOfTriangles:(triangle *)firstPtr size:(int)sz;

@end

@interface SGUtils : NSObject

+ (NSArray *) arrayOfIntegers:(int *)firstPtr size:(int)sz;
+ (void) getIntegers:(int *)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz;

@end