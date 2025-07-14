/** @type {import('next').NextConfig} */
const repo = 'tauBayesW';
const isProd = process.env.NODE_ENV === 'production';

const nextConfig = {
  output: 'export',
  basePath: isProd ? `/${repo}` : '',
  assetPrefix: isProd ? `/${repo}` : '',
  trailingSlash: true,
  eslint: { ignoreDuringBuilds: true },
  typescript: { ignoreBuildErrors: true },
  images: { 
    unoptimized: true,
    remotePatterns: [],
    domains: [],
  },
};

export default nextConfig;
