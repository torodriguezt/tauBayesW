/** @type {import('next').NextConfig} */
const repo = 'TauBayesW';          // ← GitHub repository name

const nextConfig = {
  output: 'export',
  basePath: `/${repo}`,            // 👈 add these two lines
  assetPrefix: `/${repo}/`,
  trailingSlash: true,             // nice-to-have for static hosting
  eslint: { ignoreDuringBuilds: true },
  typescript: { ignoreBuildErrors: true },
  images: { unoptimized: true },
};

export default nextConfig;
