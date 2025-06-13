# Automatic Class Schedule Generation System

This project provides a robust solution for the automatic generation of class schedules, specifically designed for the Computer Engineering program at the Costa Rica Institute of Technology, San Carlos Campus. The system leverages a Prolog backend for constraint logic and an interactive React frontend for user interaction.

## Table of Contents
- [General Description](#general-description)
- [Key Features](#key-features)
- [System Architecture](#system-architecture)
  - [Backend](#backend)
  - [Frontend](#frontend)
- [Data Structure](#data-structure)
- [Installation and Setup](#installation-and-setup)
  - [Prerequisites](#prerequisites)
  - [Backend Setup](#backend-setup)
  - [Frontend Setup](#frontend-setup)
- [Usage Guide](#usage-guide)
  - [API Endpoints](#api-endpoints)
- [Technical Challenges](#technical-challenges)
- [Future Enhancements](#future-enhancements)
- [Author](#author)

## General Description

The core of this system is a logic engine programmed in Prolog, capable of solving the complex constraints of assigning courses, professors, classrooms, and time slots to generate up to 3 viable, conflict-free schedule solutions. User interaction is handled through a modern web application that consumes a RESTful API service developed in FastAPI (Python).

## Key Features

* **Automatic Schedule Generation**: Creates complete and logically conflict-free schedules.
* **Two Generation Modes**:
    1.  **By Courses**: Allows the user to select a set of courses and generates possible schedules for that selection.
    2.  **By Semester**: Automatically generates a schedule for all courses of a given semester (odd or even).
* **Intuitive User Interface**: Developed with React and TypeScript, it offers a fluid user experience for selecting courses and visualizing the generated schedules.
* **Clear Visualization**: Displays up to 3 schedule options in an easy-to-read weekly calendar component (`react-big-calendar`).
* **Dynamic Data Management**: Professor and course information is dynamically loaded from an SQLite database into the Prolog engine when the server starts.

## System Architecture

### Backend

The backend is built with **Python** and the **FastAPI** framework, providing a RESTful API for communication with the frontend.

* **API Server**: `app.py` contains the API endpoints. It uses FastAPI for high performance.
* **Scheduling Logic**: The `scheduling.pl` file contains all the constraint logic and rules in **Prolog**. It defines the rules to avoid conflicts between schedules, classrooms, and professors.
* **Database**: **SQLite** (`scheduling.db`) is used to persist course and professor information. Initial data is hardcoded in `app.py` for a first-time setup.
* **Python-Prolog Integration**: The `pyswip` library is used as a bridge to make queries from Python to the Prolog engine. On startup, the server loads data from SQLite and asserts it as facts in the Prolog knowledge base.

### Frontend

The frontend is a Single Page Application (SPA) developed with **React** and **TypeScript**, using **Vite** as the build tool.

* **Main Components**:
    * `HomePage.tsx`: Allows users to select individual courses to generate a custom schedule.
    * `SemesterSchedulePage.tsx`: Offers the option to generate a full schedule for an odd or even semester.
    * `ScheduleDisplay.tsx`: Renders the generated schedule options in a visual calendar.
    * `CourseSelection.tsx`: Manages the course selection by the user.
* **Backend Communication**: `axios` is used to make requests to the backend API to fetch course data and the generated schedules.
* **Styling**: The interface is styled with **TailwindCSS** for a modern and responsive design.

## Data Structure

* **Professors**: Name, ID, available schedules, and the courses they can teach (max 3 per semester).
* **Courses**: Name, required classroom type, credits, and the semester it belongs to.
* **Classrooms**: Name, number, and capacity.
* **Schedules**: Time slots are managed from Monday to Friday, from 7:00-11:30 and 12:30-16:00.

## Installation and Setup

### Prerequisites

* Python 3.8+
* SWI-Prolog
* Node.js and npm (or yarn)

### Backend Setup

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/rooseveltalej/Automatic-Class-Schedule-Generation-System
    cd Automatic-Class-Schedule-Generation-System/backend
    ```

2.  **Create a virtual environment and install dependencies:**
    ```bash
    python -m venv venv
    source venv/bin/activate  # On Windows: venv\Scripts\activate
    pip install -r requirements.txt
    ```
    Dependencies include `fastapi`, `uvicorn`, and `pyswip`.

3.  **Start the backend server:**
    ```bash
    uvicorn app:app --reload
    ```
    The server will be available at `http://127.0.0.1:8000`.

4.  **Initialize the Database and Prolog (only the first time!):**
    Once the server is running, open your browser and visit the following URLs in order to populate the database and load the data into Prolog.
    -   `http://127.0.0.1:8000/insert_professors/`
    -   `http://127.0.0.1:8000/insert_courses/`
    -   `http://127.0.0.1:8000/load_professors_into_prolog/`
    -   `http://127.0.0.1:8000/load_courses_into_prolog/`

    *Note: The last two endpoints must be executed every time the server is restarted to load the data into Prolog's memory*.

### Frontend Setup

1.  **Navigate to the frontend folder:**
    ```bash
    cd ../frontend
    ```

2.  **Install dependencies:**
    ```bash
    npm install
    ```

3.  **Start the React application:**
    ```bash
    npm run dev
    ```
    The application will be available at `http://localhost:5173` (or the port Vite assigns).

## Usage Guide

Once both the backend and frontend servers are running, you can access the application through the frontend URL.

1.  **Generate schedule by courses**: On the home page, select the courses you want and click "Generate Schedule". Up to 3 compatible schedule options will be displayed.
2.  **Generate schedule by semester**: Go to the "Semester Schedule" page, choose between odd or even semester, and click "Get Schedule".

### API Endpoints

The FastAPI backend exposes several endpoints for data management and schedule generation:

* `GET /get_courses/`: Returns a list of all available courses from the database.
* `GET /get_professors/`: Returns a list of all professors.
* `GET /get_schedule/?courses=<course1>&courses=<course2>`: Generates a schedule for a specified list of courses.
* `GET /generate_schedule/?parity=<odd|even>`: Generates a schedule for all courses in an odd or even semester.
* `GET /clear_prolog_facts/`: Deletes all `course` and `professor` facts from the Prolog session.
* `GET /list_prolog_facts/`: Lists the facts currently loaded in Prolog for debugging purposes.

## Technical Challenges

* **Constraint Logic in Prolog**: The efficient implementation of constraint logic programming in Prolog.
* **Backend-Frontend Integration**: Integration of the Prolog backend with a user-friendly frontend.
* **Schedule Optimization**: Optimization of the schedule generation to provide the best possible solutions.

## Future Enhancements

* Implementation of additional constraints (e.g., professor preferences, room equipment).
* Extension to handle multiple academic programs or departments.
* Development of a web-based interface for wider accessibility.
