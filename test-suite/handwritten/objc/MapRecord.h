// AUTOGENERATED FILE - DO NOT MODIFY!
// This file generated by Djinni from map.djinni

#import <Foundation/Foundation.h>

@interface MapRecord : NSObject
- (nonnull instancetype)initWithMap:(nonnull NSDictionary *)map
                               imap:(nonnull NSDictionary *)imap;
+ (nonnull instancetype)mapRecordWithMap:(nonnull NSDictionary *)map
                                    imap:(nonnull NSDictionary *)imap;

@property (nonatomic, readonly, nonnull) NSDictionary * map;

@property (nonatomic, readonly, nonnull) NSDictionary * imap;

@end