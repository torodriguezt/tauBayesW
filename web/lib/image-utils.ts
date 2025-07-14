/**
 * Get the correct image path for both development and production
 * For GitHub Pages, we need to manually add the basePath for images
 */
export function getImageSrc(imagePath: string): string {
  // Remove leading slash if present
  const cleanPath = imagePath.startsWith('/') ? imagePath.slice(1) : imagePath;
  
  // Check if we're running in development (localhost)
  if (typeof window !== 'undefined') {
    const isLocalhost = window.location.hostname === 'localhost' || 
                       window.location.hostname === '127.0.0.1' ||
                       window.location.hostname === '';
    
    if (isLocalhost) {
      return `/${cleanPath}`;
    }
  }
  
  // For production (GitHub Pages), always use the basePath
  return `/tauBayesW/${cleanPath}`;
}
