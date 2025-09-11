import axios, { InternalAxiosRequestConfig } from 'axios';

import { API_URL } from '@/config/constants';

function authRequestInterceptor(
  config: InternalAxiosRequestConfig
): InternalAxiosRequestConfig {
  if (config.headers) {
    config.headers.Accept = 'application/json';
  }

  return config;
}

export const api = axios.create({
  baseURL: API_URL,
});

api.interceptors.request.use(authRequestInterceptor);
api.interceptors.response.use(
  (response) => {
    return response.data;
  },
  (error) => {
    const message = error.response?.data?.message || error.message;
    console.error('API Error:', message);

    return Promise.reject(error);
  }
);