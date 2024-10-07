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


/**
 * 
 * @export
 */
export const NotifPrio = {
    NpLow: 'NPLow',
    NpMedium: 'NPMedium',
    NpHigh: 'NPHigh'
} as const;
export type NotifPrio = typeof NotifPrio[keyof typeof NotifPrio];


export function instanceOfNotifPrio(value: any): boolean {
    for (const key in NotifPrio) {
        if (Object.prototype.hasOwnProperty.call(NotifPrio, key)) {
            if ((NotifPrio as Record<string, NotifPrio>)[key] === value) {
                return true;
            }
        }
    }
    return false;
}

export function NotifPrioFromJSON(json: any): NotifPrio {
    return NotifPrioFromJSONTyped(json, false);
}

export function NotifPrioFromJSONTyped(json: any, ignoreDiscriminator: boolean): NotifPrio {
    return json as NotifPrio;
}

export function NotifPrioToJSON(value?: NotifPrio | null): any {
    return value as any;
}
