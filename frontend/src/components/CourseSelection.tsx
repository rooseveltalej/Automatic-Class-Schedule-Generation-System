import React, { useState } from 'react';
import { Course } from '../api/courseApi';

interface CourseSelectionProps {
  courses: Course[];
  onSubmit: (selectedCourses: string[]) => void;
}

const CourseSelection: React.FC<CourseSelectionProps> = ({ courses, onSubmit }) => {
  const [selected, setSelected] = useState<string[]>([]);

  const handleCheckboxChange = (backendName: string) => {
    setSelected(prevSelected => {
      if (prevSelected.includes(backendName)) {
        // Deseleccionar el curso
        return prevSelected.filter(course => course !== backendName);
      } else {
        // Seleccionar el curso
        return [...prevSelected, backendName];
      }
    });
  };

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (selected.length === 0) {
      alert('Please select at least one course.');
      return;
    }
    console.log('Submitting Courses:', selected); // Log de depuración
    onSubmit(selected);
  };

  return (
    <form onSubmit={handleSubmit}>
      <div className="grid grid-cols-1 gap-4">
        {courses.map(course => (
          <label key={course.id} className="flex items-center">
            <input
              type="checkbox"
              className="form-checkbox h-5 w-5 text-blue-600"
              checked={selected.includes(course.backendName)}
              onChange={() => handleCheckboxChange(course.backendName)}
            />
            <span className="ml-2 text-gray-700">{course.name}</span>
          </label>
        ))}
      </div>
      <button
        type="submit"
        className="mt-4 bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600"
      >
        Generate Schedule
      </button>
    </form>
  );
};

export default CourseSelection;
