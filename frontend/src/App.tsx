import React from 'react';
import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
import Header from './components/Header';
import HomePage from './pages/HomePage';
import SemesterSchedulePage from './pages/SemesterSchedulePage';

function App() {
  return (
    <Router>
      <div className="min-h-screen bg-gray-100">
        <Header />
        <main className="container mx-auto px-4 py-8">
          <Routes>
            <Route path="/" element={<HomePage />} />
            <Route path="/semester-schedule" element={<SemesterSchedulePage />} />
          </Routes>
        </main>
      </div>
    </Router>
  );
}

export default App;