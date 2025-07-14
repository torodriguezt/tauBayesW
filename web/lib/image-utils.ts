/**
 * Get the correct image path for both development and production
 */
export function getImageSrc(imagePath: string): string {
  // Remove leading slash if present
  const cleanPath = imagePath.startsWith('/') ? imagePath.slice(1) : imagePath;
  
  // In production (GitHub Pages), add the basePath manually
  if (typeof window !== 'undefined' && window.location.hostname.includes('github.io')) {
    return `/tauBayesW/${cleanPath}`;
  }
  
  // In development, use the path as-is
  return `/${cleanPath}`;
}
