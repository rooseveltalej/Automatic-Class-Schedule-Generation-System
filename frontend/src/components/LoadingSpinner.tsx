import React from 'react';
import { Loader } from 'lucide-react';

const LoadingSpinner: React.FC = () => {
  return (
    <div className="flex justify-center items-center">
      <Loader className="animate-spin text-blue-500" size={24} />
      <span className="ml-2">Loading...</span>
    </div>
  );
};

export default LoadingSpinner;