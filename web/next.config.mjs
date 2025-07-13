/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',          // ⬅️  This makes `next build` create /out
  eslint: { ignoreDuringBuilds: true },
  typescript: { ignoreBuildErrors: true },
  images: { unoptimized: true },
};

export default nextConfig;
