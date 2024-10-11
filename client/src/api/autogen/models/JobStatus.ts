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
export const JobStatus = {
    JobPending: 'JobPending',
    JobRunning: 'JobRunning',
    JobFinished: 'JobFinished',
    JobFailed: 'JobFailed'
} as const;
export type JobStatus = typeof JobStatus[keyof typeof JobStatus];


export function instanceOfJobStatus(value: any): boolean {
    for (const key in JobStatus) {
        if (Object.prototype.hasOwnProperty.call(JobStatus, key)) {
            if ((JobStatus as Record<string, JobStatus>)[key] === value) {
                return true;
            }
        }
    }
    return false;
}

export function JobStatusFromJSON(json: any): JobStatus {
    return JobStatusFromJSONTyped(json, false);
}

export function JobStatusFromJSONTyped(json: any, ignoreDiscriminator: boolean): JobStatus {
    return json as JobStatus;
}

export function JobStatusToJSON(value?: JobStatus | null): any {
    return value as any;
}
