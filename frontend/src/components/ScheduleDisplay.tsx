import React from 'react';

interface ScheduleItem {
  course: string;
  professor: string;
  room: string;
  day: string;
  start: number;
  end: number;
}

interface ScheduleDisplayProps {
  schedule: ScheduleItem[][];
}

const ScheduleDisplay: React.FC<ScheduleDisplayProps> = ({ schedule }) => {
  return (
    <div className="space-y-8">
      {schedule.map((option, index) => (
        <div key={index} className="border p-4 rounded shadow">
          <h3 className="text-xl font-semibold mb-2">Option {index + 1}</h3>
          <table className="min-w-full bg-white">
            <thead>
              <tr>
                <th className="py-2 px-4 border-b">Course</th>
                <th className="py-2 px-4 border-b">Professor</th>
                <th className="py-2 px-4 border-b">Room</th>
                <th className="py-2 px-4 border-b">Day</th>
                <th className="py-2 px-4 border-b">Start</th>
                <th className="py-2 px-4 border-b">End</th>
              </tr>
            </thead>
            <tbody>
              {option.map((item, idx) => (
                <tr key={idx}>
                  <td className="py-2 px-4 border-b">{item.course}</td>
                  <td className="py-2 px-4 border-b">{item.professor}</td>
                  <td className="py-2 px-4 border-b">{item.room}</td>
                  <td className="py-2 px-4 border-b capitalize">{item.day}</td>
                  <td className="py-2 px-4 border-b">{item.start}:00</td>
                  <td className="py-2 px-4 border-b">{item.end}:00</td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      ))}
    </div>
  );
};

export default ScheduleDisplay;
