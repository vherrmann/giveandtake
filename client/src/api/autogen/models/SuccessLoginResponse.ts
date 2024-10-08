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
import type { User } from './User';
import {
    UserFromJSON,
    UserFromJSONTyped,
    UserToJSON,
} from './User';

/**
 * 
 * @export
 * @interface SuccessLoginResponse
 */
export interface SuccessLoginResponse {
    /**
     * 
     * @type {User}
     * @memberof SuccessLoginResponse
     */
    user: User;
    /**
     * 
     * @type {string}
     * @memberof SuccessLoginResponse
     */
    userId: string;
}

/**
 * Check if a given object implements the SuccessLoginResponse interface.
 */
export function instanceOfSuccessLoginResponse(value: object): value is SuccessLoginResponse {
    if (!('user' in value) || value['user'] === undefined) return false;
    if (!('userId' in value) || value['userId'] === undefined) return false;
    return true;
}

export function SuccessLoginResponseFromJSON(json: any): SuccessLoginResponse {
    return SuccessLoginResponseFromJSONTyped(json, false);
}

export function SuccessLoginResponseFromJSONTyped(json: any, ignoreDiscriminator: boolean): SuccessLoginResponse {
    if (json == null) {
        return json;
    }
    return {
        
        'user': UserFromJSON(json['user']),
        'userId': json['userId'],
    };
}

export function SuccessLoginResponseToJSON(value?: SuccessLoginResponse | null): any {
    if (value == null) {
        return value;
    }
    return {
        
        'user': UserToJSON(value['user']),
        'userId': value['userId'],
    };
}

