// src/pages/SemesterSchedulePage.tsx

import React, { useState } from 'react';
import ScheduleDisplay from '../components/ScheduleDisplay';
import LoadingSpinner from '../components/LoadingSpinner';
import { getSemesterSchedule } from '../api/scheduleApi';

const SemesterSchedulePage: React.FC = () => {
  const [semester, setSemester] = useState<'odd' | 'even'>('odd');
  const [schedule, setSchedule] = useState<any[][]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setError(null);
    try {
      const semesterSchedule = await getSemesterSchedule(semester);
      setSchedule(semesterSchedule);
    } catch (err) {
      setError('Failed to retrieve semester schedule. Please try again.');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="space-y-8">
      <section>
        <h2 className="text-2xl font-bold mb-4">Semester Schedule</h2>
        <form onSubmit={handleSubmit} className="space-y-4">
          <div>
            <label className="block mb-2">Select Semester:</label>
            <select
              value={semester}
              onChange={(e) => setSemester(e.target.value as 'odd' | 'even')}
              className="form-select mt-1 block w-full"
            >
              <option value="odd">Odd Semester</option>
              <option value="even">Even Semester</option>
            </select>
          </div>
          <button
            type="submit"
            className="bg-blue-500 hover:bg-blue-600 text-white font-bold py-2 px-4 rounded"
          >
            Get Schedule
          </button>
        </form>
      </section>

      {loading && <LoadingSpinner />}

      {error && (
        <div className="bg-red-100 border border-red-400 text-red-700 px-4 py-3 rounded relative" role="alert">
          <strong className="font-bold">Error!</strong>
          <span className="block sm:inline"> {error}</span>
        </div>
      )}

      {schedule.length > 0 && (
        <section>
          <h2 className="text-2xl font-bold mb-4">Semester Schedule Options</h2>
          <ScheduleDisplay schedule={schedule} />
        </section>
      )}

      {schedule.length === 0 && !loading && !error && (
        <p className="text-gray-600">No schedule retrieved yet. Please select a semester and submit.</p>
      )}
    </div>
  );
};

export default SemesterSchedulePage;
