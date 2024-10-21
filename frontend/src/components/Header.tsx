import React from 'react';
import { Link } from 'react-router-dom';
import { Calendar, Home } from 'lucide-react';

const Header: React.FC = () => {
  return (
    <header className="bg-blue-600 text-white shadow-md">
      <div className="container mx-auto px-4 py-4 flex justify-between items-center">
        <h1 className="text-2xl font-bold">Course Scheduler</h1>
        <nav>
          <ul className="flex space-x-4">
            <li>
              <Link to="/" className="flex items-center hover:text-blue-200">
                <Home className="mr-1" size={18} />
                Home
              </Link>
            </li>
            <li>
              <Link to="/semester-schedule" className="flex items-center hover:text-blue-200">
                <Calendar className="mr-1" size={18} />
                Semester Schedule
              </Link>
            </li>
          </ul>
        </nav>
      </div>
    </header>
  );
};

export default Header;