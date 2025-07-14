/**
 * Utility function to get the correct asset path for both development and production
 * In development: returns the path as-is
 * In production (GitHub Pages): prepends the basePath
 */
export function getAssetPath(path: string): string {
  // Remove leading slash if present
  const cleanPath = path.startsWith('/') ? path.slice(1) : path;
  
  // In production, use the basePath from Next.js config
  const basePath = process.env.NODE_ENV === 'production' ? '/tauBayesW' : '';
  
  return `${basePath}/${cleanPath}`;
}

/**
 * Get the logo path with fallback
 */
export function getLogoPath(): string {
  return getAssetPath('logo_tau.png');
}
