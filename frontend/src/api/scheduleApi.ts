import axios from 'axios';

export const generateSchedule = async (selectedCourses: string[]): Promise<any[][]> => {
  const params = new URLSearchParams();
  selectedCourses.forEach(course => params.append('courses', course));

  const response = await axios.get('http://localhost:8000/get_schedule/', { params });

  // La respuesta esperada es una matriz de matrices
  return response.data;
};

export const getSemesterSchedule = async (parity: 'even' | 'odd'): Promise<any[]> => {
  try {
    const response = await fetch(`http://localhost:8000/generate_schedule/?parity=${parity}`);
    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.detail || 'Failed to fetch schedule');
    }
    const data = await response.json();
    if (data.schedules) {
      return data.schedules;
    } else {
      throw new Error(data.message || 'No schedule found');
    }
  } catch (error) {
    throw error;
  }
};

