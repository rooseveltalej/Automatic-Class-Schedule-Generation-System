import axios from 'axios';

export interface Course {
  id: string;
  name: string;
  backendName: string;
  type: string;
  credits: number;
  semester: number;
}

export const fetchCourses = async (): Promise<Course[]> => {
  const response = await axios.get('http://localhost:8000/get_courses/');
  // Verifica si `backendName` está presente
  if (response.data && response.data.length > 0 && !response.data[0].backendName) {
    // Si no está, mapea los cursos para añadirlo
    return response.data.map((course: any) => ({
      ...course,
      backendName: course.name.toLowerCase().replace(/\s+/g, '_'),
    }));
  }
  return response.data;
};
