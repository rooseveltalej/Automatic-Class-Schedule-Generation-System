import axios from 'axios';

export const generateSchedule = async (selectedCourses: string[]): Promise<any[][]> => {
  const params = new URLSearchParams();
  selectedCourses.forEach(course => params.append('courses', course));

  const response = await axios.get('http://localhost:8000/get_schedule/', { params });

  // La respuesta esperada es una matriz de matrices
  return response.data;
};

export const getSemesterSchedule = async (semester: 'odd' | 'even') => {
  try {
    const response = await axios.get(`${API_BASE_URL}/semester-schedule`, { params: { semester } });
    return response.data;
  } catch (error) {
    console.error('Error fetching semester schedule:', error);
    throw error;
  }
};

