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


import * as runtime from '../runtime';
import type {
  CheckResponse,
  FeedType,
  FeedUrlPostResponse,
  FriendsRequestGetResponse,
  JobStatus,
  LoginData,
  NewPost,
  Post,
  SignupData,
  SuccessLoginResponse,
  UploadMediaResponse,
  UserPublic,
  VerifyEmail,
  WithUUIDNotification,
  WithUUIDPost,
  WithUUIDUserPublic,
} from '../models/index';
import {
    CheckResponseFromJSON,
    CheckResponseToJSON,
    FeedTypeFromJSON,
    FeedTypeToJSON,
    FeedUrlPostResponseFromJSON,
    FeedUrlPostResponseToJSON,
    FriendsRequestGetResponseFromJSON,
    FriendsRequestGetResponseToJSON,
    JobStatusFromJSON,
    JobStatusToJSON,
    LoginDataFromJSON,
    LoginDataToJSON,
    NewPostFromJSON,
    NewPostToJSON,
    PostFromJSON,
    PostToJSON,
    SignupDataFromJSON,
    SignupDataToJSON,
    SuccessLoginResponseFromJSON,
    SuccessLoginResponseToJSON,
    UploadMediaResponseFromJSON,
    UploadMediaResponseToJSON,
    UserPublicFromJSON,
    UserPublicToJSON,
    VerifyEmailFromJSON,
    VerifyEmailToJSON,
    WithUUIDNotificationFromJSON,
    WithUUIDNotificationToJSON,
    WithUUIDPostFromJSON,
    WithUUIDPostToJSON,
    WithUUIDUserPublicFromJSON,
    WithUUIDUserPublicToJSON,
} from '../models/index';

export interface ApiAuthLoginPostRequest {
    loginData: LoginData;
}

export interface ApiAuthSignupPostRequest {
    signupData: SignupData;
}

export interface ApiAuthVerifyemailPostRequest {
    verifyEmail: VerifyEmail;
}

export interface ApiFeedIdTokenGetRequest {
    id: string;
    token: string;
    accept?: string;
}

export interface ApiFeedUrlPostRequest {
    body: string;
}

export interface ApiFriendsFriendIdDeleteRequest {
    friendId: string;
}

export interface ApiFriendsRequestFriendIdAcceptPostRequest {
    friendId: string;
}

export interface ApiFriendsRequestFriendIdPostRequest {
    friendId: string;
}

export interface ApiFriendsRequestFriendIdRejectPostRequest {
    friendId: string;
}

export interface ApiJobIdResultMediaCompressGetRequest {
    id: string;
}

export interface ApiJobIdResultVerifyEmailGetRequest {
    id: string;
}

export interface ApiJobIdStatusGetRequest {
    id: string;
}

export interface ApiNotifReadPostRequest {
    requestBody: Array<string>;
}

export interface ApiPostsIdDeleteRequest {
    id: string;
}

export interface ApiPostsIdGetRequest {
    id: string;
}

export interface ApiPostsPostRequest {
    newPost: NewPost;
}

export interface ApiUsersIdGetRequest {
    id: string;
}

export interface ApiUsersIdPostsGetRequest {
    id: string;
}

/**
 * 
 */
export class DefaultApi extends runtime.BaseAPI {

    /**
     */
    async apiAuthCheckGetRaw(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<CheckResponse>> {
        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/auth/check`,
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => CheckResponseFromJSON(jsonValue));
    }

    /**
     */
    async apiAuthCheckGet(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<CheckResponse> {
        const response = await this.apiAuthCheckGetRaw(initOverrides);
        return await response.value();
    }

    /**
     */
    async apiAuthLoginPostRaw(requestParameters: ApiAuthLoginPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<SuccessLoginResponse>> {
        if (requestParameters['loginData'] == null) {
            throw new runtime.RequiredError(
                'loginData',
                'Required parameter "loginData" was null or undefined when calling apiAuthLoginPost().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        headerParameters['Content-Type'] = 'application/json;charset=utf-8';

        const response = await this.request({
            path: `/api/auth/login`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
            body: LoginDataToJSON(requestParameters['loginData']),
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => SuccessLoginResponseFromJSON(jsonValue));
    }

    /**
     */
    async apiAuthLoginPost(requestParameters: ApiAuthLoginPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<SuccessLoginResponse> {
        const response = await this.apiAuthLoginPostRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiAuthLogoutPostRaw(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<void>> {
        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        const response = await this.request({
            path: `/api/auth/logout`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.VoidApiResponse(response);
    }

    /**
     */
    async apiAuthLogoutPost(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<void> {
        await this.apiAuthLogoutPostRaw(initOverrides);
    }

    /**
     */
    async apiAuthSignupPostRaw(requestParameters: ApiAuthSignupPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<string>> {
        if (requestParameters['signupData'] == null) {
            throw new runtime.RequiredError(
                'signupData',
                'Required parameter "signupData" was null or undefined when calling apiAuthSignupPost().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        headerParameters['Content-Type'] = 'application/json;charset=utf-8';

        const response = await this.request({
            path: `/api/auth/signup`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
            body: SignupDataToJSON(requestParameters['signupData']),
        }, initOverrides);

        if (this.isJsonMime(response.headers.get('content-type'))) {
            return new runtime.JSONApiResponse<string>(response);
        } else {
            return new runtime.TextApiResponse(response) as any;
        }
    }

    /**
     */
    async apiAuthSignupPost(requestParameters: ApiAuthSignupPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<string> {
        const response = await this.apiAuthSignupPostRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiAuthVerifyemailPostRaw(requestParameters: ApiAuthVerifyemailPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<any>>> {
        if (requestParameters['verifyEmail'] == null) {
            throw new runtime.RequiredError(
                'verifyEmail',
                'Required parameter "verifyEmail" was null or undefined when calling apiAuthVerifyemailPost().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        headerParameters['Content-Type'] = 'application/json;charset=utf-8';

        const response = await this.request({
            path: `/api/auth/verifyemail`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
            body: VerifyEmailToJSON(requestParameters['verifyEmail']),
        }, initOverrides);

        return new runtime.JSONApiResponse<any>(response);
    }

    /**
     */
    async apiAuthVerifyemailPost(requestParameters: ApiAuthVerifyemailPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<any>> {
        const response = await this.apiAuthVerifyemailPostRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiFeedIdTokenGetRaw(requestParameters: ApiFeedIdTokenGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<any>> {
        if (requestParameters['id'] == null) {
            throw new runtime.RequiredError(
                'id',
                'Required parameter "id" was null or undefined when calling apiFeedIdTokenGet().'
            );
        }

        if (requestParameters['token'] == null) {
            throw new runtime.RequiredError(
                'token',
                'Required parameter "token" was null or undefined when calling apiFeedIdTokenGet().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (requestParameters['accept'] != null) {
            headerParameters['Accept'] = String(requestParameters['accept']);
        }

        const response = await this.request({
            path: `/api/feed/{id}/{token}`.replace(`{${"id"}}`, encodeURIComponent(String(requestParameters['id']))).replace(`{${"token"}}`, encodeURIComponent(String(requestParameters['token']))),
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        if (this.isJsonMime(response.headers.get('content-type'))) {
            return new runtime.JSONApiResponse<any>(response);
        } else {
            return new runtime.TextApiResponse(response) as any;
        }
    }

    /**
     */
    async apiFeedIdTokenGet(requestParameters: ApiFeedIdTokenGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<any> {
        const response = await this.apiFeedIdTokenGetRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiFeedUrlPostRaw(requestParameters: ApiFeedUrlPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<FeedUrlPostResponse>> {
        if (requestParameters['body'] == null) {
            throw new runtime.RequiredError(
                'body',
                'Required parameter "body" was null or undefined when calling apiFeedUrlPost().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        headerParameters['Content-Type'] = 'application/json;charset=utf-8';

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/feed/url`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
            body: requestParameters['body'] as any,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => FeedUrlPostResponseFromJSON(jsonValue));
    }

    /**
     */
    async apiFeedUrlPost(requestParameters: ApiFeedUrlPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<FeedUrlPostResponse> {
        const response = await this.apiFeedUrlPostRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiFriendsFriendIdDeleteRaw(requestParameters: ApiFriendsFriendIdDeleteRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<any>>> {
        if (requestParameters['friendId'] == null) {
            throw new runtime.RequiredError(
                'friendId',
                'Required parameter "friendId" was null or undefined when calling apiFriendsFriendIdDelete().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/friends/{friendId}`.replace(`{${"friendId"}}`, encodeURIComponent(String(requestParameters['friendId']))),
            method: 'DELETE',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse<any>(response);
    }

    /**
     */
    async apiFriendsFriendIdDelete(requestParameters: ApiFriendsFriendIdDeleteRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<any>> {
        const response = await this.apiFriendsFriendIdDeleteRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiFriendsGetRaw(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<WithUUIDUserPublic>>> {
        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/friends`,
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => jsonValue.map(WithUUIDUserPublicFromJSON));
    }

    /**
     */
    async apiFriendsGet(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<WithUUIDUserPublic>> {
        const response = await this.apiFriendsGetRaw(initOverrides);
        return await response.value();
    }

    /**
     */
    async apiFriendsRequestFriendIdAcceptPostRaw(requestParameters: ApiFriendsRequestFriendIdAcceptPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<any>>> {
        if (requestParameters['friendId'] == null) {
            throw new runtime.RequiredError(
                'friendId',
                'Required parameter "friendId" was null or undefined when calling apiFriendsRequestFriendIdAcceptPost().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/friends/request/{friendId}/accept`.replace(`{${"friendId"}}`, encodeURIComponent(String(requestParameters['friendId']))),
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse<any>(response);
    }

    /**
     */
    async apiFriendsRequestFriendIdAcceptPost(requestParameters: ApiFriendsRequestFriendIdAcceptPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<any>> {
        const response = await this.apiFriendsRequestFriendIdAcceptPostRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiFriendsRequestFriendIdPostRaw(requestParameters: ApiFriendsRequestFriendIdPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<any>>> {
        if (requestParameters['friendId'] == null) {
            throw new runtime.RequiredError(
                'friendId',
                'Required parameter "friendId" was null or undefined when calling apiFriendsRequestFriendIdPost().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/friends/request/{friendId}`.replace(`{${"friendId"}}`, encodeURIComponent(String(requestParameters['friendId']))),
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse<any>(response);
    }

    /**
     */
    async apiFriendsRequestFriendIdPost(requestParameters: ApiFriendsRequestFriendIdPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<any>> {
        const response = await this.apiFriendsRequestFriendIdPostRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiFriendsRequestFriendIdRejectPostRaw(requestParameters: ApiFriendsRequestFriendIdRejectPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<any>>> {
        if (requestParameters['friendId'] == null) {
            throw new runtime.RequiredError(
                'friendId',
                'Required parameter "friendId" was null or undefined when calling apiFriendsRequestFriendIdRejectPost().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/friends/request/{friendId}/reject`.replace(`{${"friendId"}}`, encodeURIComponent(String(requestParameters['friendId']))),
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse<any>(response);
    }

    /**
     */
    async apiFriendsRequestFriendIdRejectPost(requestParameters: ApiFriendsRequestFriendIdRejectPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<any>> {
        const response = await this.apiFriendsRequestFriendIdRejectPostRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiFriendsRequestGetRaw(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<FriendsRequestGetResponse>> {
        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/friends/request`,
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => FriendsRequestGetResponseFromJSON(jsonValue));
    }

    /**
     */
    async apiFriendsRequestGet(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<FriendsRequestGetResponse> {
        const response = await this.apiFriendsRequestGetRaw(initOverrides);
        return await response.value();
    }

    /**
     */
    async apiJobIdResultMediaCompressGetRaw(requestParameters: ApiJobIdResultMediaCompressGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<UploadMediaResponse>> {
        if (requestParameters['id'] == null) {
            throw new runtime.RequiredError(
                'id',
                'Required parameter "id" was null or undefined when calling apiJobIdResultMediaCompressGet().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/job/{id}/result/mediaCompress`.replace(`{${"id"}}`, encodeURIComponent(String(requestParameters['id']))),
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => UploadMediaResponseFromJSON(jsonValue));
    }

    /**
     */
    async apiJobIdResultMediaCompressGet(requestParameters: ApiJobIdResultMediaCompressGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<UploadMediaResponse> {
        const response = await this.apiJobIdResultMediaCompressGetRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiJobIdResultVerifyEmailGetRaw(requestParameters: ApiJobIdResultVerifyEmailGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<any>>> {
        if (requestParameters['id'] == null) {
            throw new runtime.RequiredError(
                'id',
                'Required parameter "id" was null or undefined when calling apiJobIdResultVerifyEmailGet().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/job/{id}/result/verifyEmail`.replace(`{${"id"}}`, encodeURIComponent(String(requestParameters['id']))),
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse<any>(response);
    }

    /**
     */
    async apiJobIdResultVerifyEmailGet(requestParameters: ApiJobIdResultVerifyEmailGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<any>> {
        const response = await this.apiJobIdResultVerifyEmailGetRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiJobIdStatusGetRaw(requestParameters: ApiJobIdStatusGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<JobStatus>> {
        if (requestParameters['id'] == null) {
            throw new runtime.RequiredError(
                'id',
                'Required parameter "id" was null or undefined when calling apiJobIdStatusGet().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/job/{id}/status`.replace(`{${"id"}}`, encodeURIComponent(String(requestParameters['id']))),
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => JobStatusFromJSON(jsonValue));
    }

    /**
     */
    async apiJobIdStatusGet(requestParameters: ApiJobIdStatusGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<JobStatus> {
        const response = await this.apiJobIdStatusGetRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiMediaUploadPostRaw(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<string>> {
        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/media/upload`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        if (this.isJsonMime(response.headers.get('content-type'))) {
            return new runtime.JSONApiResponse<string>(response);
        } else {
            return new runtime.TextApiResponse(response) as any;
        }
    }

    /**
     */
    async apiMediaUploadPost(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<string> {
        const response = await this.apiMediaUploadPostRaw(initOverrides);
        return await response.value();
    }

    /**
     */
    async apiNotifGetRaw(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<WithUUIDNotification>>> {
        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/notif`,
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => jsonValue.map(WithUUIDNotificationFromJSON));
    }

    /**
     */
    async apiNotifGet(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<WithUUIDNotification>> {
        const response = await this.apiNotifGetRaw(initOverrides);
        return await response.value();
    }

    /**
     */
    async apiNotifReadPostRaw(requestParameters: ApiNotifReadPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<any>>> {
        if (requestParameters['requestBody'] == null) {
            throw new runtime.RequiredError(
                'requestBody',
                'Required parameter "requestBody" was null or undefined when calling apiNotifReadPost().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        headerParameters['Content-Type'] = 'application/json;charset=utf-8';

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/notif/read`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
            body: requestParameters['requestBody'],
        }, initOverrides);

        return new runtime.JSONApiResponse<any>(response);
    }

    /**
     */
    async apiNotifReadPost(requestParameters: ApiNotifReadPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<any>> {
        const response = await this.apiNotifReadPostRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiPostsFeedGetRaw(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<WithUUIDPost>>> {
        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/posts/feed`,
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => jsonValue.map(WithUUIDPostFromJSON));
    }

    /**
     */
    async apiPostsFeedGet(initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<WithUUIDPost>> {
        const response = await this.apiPostsFeedGetRaw(initOverrides);
        return await response.value();
    }

    /**
     */
    async apiPostsIdDeleteRaw(requestParameters: ApiPostsIdDeleteRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<any>>> {
        if (requestParameters['id'] == null) {
            throw new runtime.RequiredError(
                'id',
                'Required parameter "id" was null or undefined when calling apiPostsIdDelete().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/posts/{id}`.replace(`{${"id"}}`, encodeURIComponent(String(requestParameters['id']))),
            method: 'DELETE',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse<any>(response);
    }

    /**
     */
    async apiPostsIdDelete(requestParameters: ApiPostsIdDeleteRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<any>> {
        const response = await this.apiPostsIdDeleteRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiPostsIdGetRaw(requestParameters: ApiPostsIdGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Post>> {
        if (requestParameters['id'] == null) {
            throw new runtime.RequiredError(
                'id',
                'Required parameter "id" was null or undefined when calling apiPostsIdGet().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/posts/{id}`.replace(`{${"id"}}`, encodeURIComponent(String(requestParameters['id']))),
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => PostFromJSON(jsonValue));
    }

    /**
     */
    async apiPostsIdGet(requestParameters: ApiPostsIdGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Post> {
        const response = await this.apiPostsIdGetRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiPostsPostRaw(requestParameters: ApiPostsPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<string>> {
        if (requestParameters['newPost'] == null) {
            throw new runtime.RequiredError(
                'newPost',
                'Required parameter "newPost" was null or undefined when calling apiPostsPost().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        headerParameters['Content-Type'] = 'application/json;charset=utf-8';

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/posts`,
            method: 'POST',
            headers: headerParameters,
            query: queryParameters,
            body: NewPostToJSON(requestParameters['newPost']),
        }, initOverrides);

        if (this.isJsonMime(response.headers.get('content-type'))) {
            return new runtime.JSONApiResponse<string>(response);
        } else {
            return new runtime.TextApiResponse(response) as any;
        }
    }

    /**
     */
    async apiPostsPost(requestParameters: ApiPostsPostRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<string> {
        const response = await this.apiPostsPostRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiUsersIdGetRaw(requestParameters: ApiUsersIdGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<UserPublic>> {
        if (requestParameters['id'] == null) {
            throw new runtime.RequiredError(
                'id',
                'Required parameter "id" was null or undefined when calling apiUsersIdGet().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/users/{id}`.replace(`{${"id"}}`, encodeURIComponent(String(requestParameters['id']))),
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => UserPublicFromJSON(jsonValue));
    }

    /**
     */
    async apiUsersIdGet(requestParameters: ApiUsersIdGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<UserPublic> {
        const response = await this.apiUsersIdGetRaw(requestParameters, initOverrides);
        return await response.value();
    }

    /**
     */
    async apiUsersIdPostsGetRaw(requestParameters: ApiUsersIdPostsGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<runtime.ApiResponse<Array<WithUUIDPost>>> {
        if (requestParameters['id'] == null) {
            throw new runtime.RequiredError(
                'id',
                'Required parameter "id" was null or undefined when calling apiUsersIdPostsGet().'
            );
        }

        const queryParameters: any = {};

        const headerParameters: runtime.HTTPHeaders = {};

        if (this.configuration && this.configuration.accessToken) {
            const token = this.configuration.accessToken;
            const tokenString = await token("Cookie", []);

            if (tokenString) {
                headerParameters["Authorization"] = `Bearer ${tokenString}`;
            }
        }
        const response = await this.request({
            path: `/api/users/{id}/posts`.replace(`{${"id"}}`, encodeURIComponent(String(requestParameters['id']))),
            method: 'GET',
            headers: headerParameters,
            query: queryParameters,
        }, initOverrides);

        return new runtime.JSONApiResponse(response, (jsonValue) => jsonValue.map(WithUUIDPostFromJSON));
    }

    /**
     */
    async apiUsersIdPostsGet(requestParameters: ApiUsersIdPostsGetRequest, initOverrides?: RequestInit | runtime.InitOverrideFunction): Promise<Array<WithUUIDPost>> {
        const response = await this.apiUsersIdPostsGetRaw(requestParameters, initOverrides);
        return await response.value();
    }

}
