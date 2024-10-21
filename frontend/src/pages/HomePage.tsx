import React, { useState, useEffect } from 'react';
import CourseSelection from '../components/CourseSelection';
import ScheduleDisplay from '../components/ScheduleDisplay';
import LoadingSpinner from '../components/LoadingSpinner';
import { generateSchedule } from '../api/scheduleApi';
import { fetchCourses, Course } from '../api/courseApi';

const HomePage: React.FC = () => {
  const [courses, setCourses] = useState<Course[]>([]);
  const [schedule, setSchedule] = useState<any[][]>([]);
  const [loading, setLoading] = useState(false);
  const [loadingCourses, setLoadingCourses] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [errorCourses, setErrorCourses] = useState<string | null>(null);

  useEffect(() => {
    const getCourses = async () => {
      setLoadingCourses(true);
      setErrorCourses(null);
      try {
        const fetchedCourses = await fetchCourses();
        setCourses(fetchedCourses);
      } catch (err) {
        setErrorCourses('Failed to load courses. Please try again.');
        console.error(err);
      } finally {
        setLoadingCourses(false);
      }
    };

    getCourses();
  }, []);

  const handleSubmit = async (selectedCourses: string[]) => {
    setLoading(true);
    setError(null);
    try {
      const generatedSchedule = await generateSchedule(selectedCourses);
      console.log('Generated Schedule:', generatedSchedule); // Para depuración
      setSchedule(generatedSchedule);
    } catch (err) {
      setError('Failed to generate schedule. Please try again.');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="space-y-8 p-8">
      <section>
        <h2 className="text-2xl font-bold mb-4">Select Courses</h2>
        {loadingCourses ? (
          <LoadingSpinner />
        ) : errorCourses ? (
          <div
            className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative"
            role="alert"
          >
            <strong className="font-bold">Error!</strong>
            <span className="block sm:inline"> {errorCourses}</span>
          </div>
        ) : (
          <CourseSelection courses={courses} onSubmit={handleSubmit} />
        )}
      </section>

      {loading && <LoadingSpinner />}

      {error && (
        <div
          className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative"
          role="alert"
        >
          <strong className="font-bold">Error!</strong>
          <span className="block sm:inline"> {error}</span>
        </div>
      )}

      {schedule.length > 0 && (
        <section>
          <h2 className="text-2xl font-bold mb-4">Generated Schedule</h2>
          <ScheduleDisplay schedule={schedule} />
        </section>
      )}

      {schedule.length === 0 && !loading && !error && (
        <p className="text-gray-600">No schedule generated yet. Please select courses and submit.</p>
      )}
    </div>
  );
};

export default HomePage;
