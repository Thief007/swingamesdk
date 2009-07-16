#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <Foundation/NSArray.h>


@interface SGUtils : NSObject

+ (NSArray *) arrayOfIntegers:(int *)firstPtr size:(int)sz;
+ (void) getIntegers:(int *)firstPtr fromArray:(NSArray *)in_data maxSize:(int)sz;

@end