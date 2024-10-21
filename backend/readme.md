# Automatic Class Schedule Generation System

## Description
This project develops an automatic system for generating class schedules for the Computer Engineering program at the San Carlos Campus of the Costa Rica Institute of Technology. The system uses Prolog for the programming logic and a user interface developed in another programming language for user interaction.

## Key Features
- Automatic generation of conflict-free schedules.
- Management of information about courses, professors, and classrooms.
- Dynamic data loading from a database.
- Generation of multiple optimized solutions.

## System Requirements
- Prolog (for programming logic)
- Database engine (for information storage)
- Additional programming language for the user interface

## Data Structure
- **Professors**: Name, ID, available schedules, courses they teach (max. 3 per semester)
- **Courses**: Name, classroom type, number of credits, semester
- **Classrooms**: Name, number, capacity
- **Schedules**: Monday to Friday, 7:00-11:30 and 12:30-16:00

## Functionalities
1. Query possible schedules for a selection of courses.
2. Automatic generation of schedules by semester (odd or even).

## User Interface
The interface allows:
- Selecting courses to generate schedules.
- Querying schedules by semester.
- Visualizing up to 3 different solutions per query.

## Installation and Usage
[Installation and usage instructions pending]

## Important Notes
- The system should optimize the search to generate the "best solutions" first.
- A maximum of 3 different solutions should be generated per query.
- Information is dynamically loaded from the database.

## Delivery
- Date: Monday, October 21, 2024, before 10:00 PM
- Platform: Tec Digital

## Author
Roosevelt Alejandro Pérez González


## Project Context
This project is part of the Programming Languages course at the Costa Rica Institute of Technology, San Carlos Campus. It aims to apply concepts of logical programming and Prolog to solve a real-world problem in academic scheduling.

## Technical Challenges
- Efficient implementation of constraint logic programming in Prolog.
- Integration of Prolog backend with a user-friendly frontend.
- Optimization of schedule generation to provide the best possible solutions.

## Future Enhancements
- Implementation of additional constraints (e.g., professor preferences, room equipment).
- Extension to handle multiple academic programs or departments.
- Development of a web-based interface for wider accessibility.
