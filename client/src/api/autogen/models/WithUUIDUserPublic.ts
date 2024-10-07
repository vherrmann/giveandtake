/* tslint:disable */
/* eslint-disable */
/**
 * GiveAndTake API
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 0.1
 * 
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 */

import { mapValues } from '../runtime';
import type { UserPublic } from './UserPublic';
import {
    UserPublicFromJSON,
    UserPublicFromJSONTyped,
    UserPublicToJSON,
} from './UserPublic';

/**
 * 
 * @export
 * @interface WithUUIDUserPublic
 */
export interface WithUUIDUserPublic {
    /**
     * 
     * @type {string}
     * @memberof WithUUIDUserPublic
     */
    uuid: string;
    /**
     * 
     * @type {UserPublic}
     * @memberof WithUUIDUserPublic
     */
    value: UserPublic;
}

/**
 * Check if a given object implements the WithUUIDUserPublic interface.
 */
export function instanceOfWithUUIDUserPublic(value: object): value is WithUUIDUserPublic {
    if (!('uuid' in value) || value['uuid'] === undefined) return false;
    if (!('value' in value) || value['value'] === undefined) return false;
    return true;
}

export function WithUUIDUserPublicFromJSON(json: any): WithUUIDUserPublic {
    return WithUUIDUserPublicFromJSONTyped(json, false);
}

export function WithUUIDUserPublicFromJSONTyped(json: any, ignoreDiscriminator: boolean): WithUUIDUserPublic {
    if (json == null) {
        return json;
    }
    return {
        
        'uuid': json['uuid'],
        'value': UserPublicFromJSON(json['value']),
    };
}

export function WithUUIDUserPublicToJSON(value?: WithUUIDUserPublic | null): any {
    if (value == null) {
        return value;
    }
    return {
        
        'uuid': value['uuid'],
        'value': UserPublicToJSON(value['value']),
    };
}
