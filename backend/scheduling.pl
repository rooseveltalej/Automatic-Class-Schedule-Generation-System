% Declarar predicados dinámicos
:- dynamic professor/3.
:- dynamic course/4.

% Rooms
room(lab1, 20, lab).
room(lab2, 25, lab).
room(lab3, 30, lab).
room(moviles, 20, normal).
room(miniauditorio, 40, normal).

% Time slots available for classes
available_time_slot(monday, 7, 1130).
available_time_slot(monday, 1230, 1600).
available_time_slot(tuesday, 7, 1130).
available_time_slot(tuesday, 1230, 1600).
available_time_slot(wednesday, 7, 1130).
available_time_slot(wednesday, 1230, 1600).
available_time_slot(thursday, 7, 1130).
available_time_slot(thursday, 1230, 1600).
available_time_slot(friday, 7, 1130).
available_time_slot(friday, 1230, 1600).

% Profesores
professor(prof_juan, [
    ['monday', 7, 1130],
    ['wednesday', 1230, 1600]
], [calculo_i, fisica_i]).

% Cursos
course(calculo_i, normal, 4, 1).     % Semestre 1 (impar)
course(fisica_i, lab, 4, 2).         % Semestre 2 (par)
course(algoritmos, lab, 4, 3).       % Semestre 3 (impar)
course(bases_de_datos, normal, 4, 4).% Semestre 4 (par)

% Predicado para determinar si un semestre es par
is_even(Semester) :-
    Semester mod 2 =:= 0.

% Predicado para determinar si un semestre es impar
is_odd(Semester) :-
    Semester mod 2 =:= 1.

% Rest of your existing rules...
% Rules for determining availability
available_professor(Professor, Day, Start, End) :-
    professor(Professor, AvailableHours, _),
    member([Day, Start, End], AvailableHours).

available_room(Room, RoomType) :-
    room(Room, _, RoomType).

can_teach(Professor, Course) :-
    professor(Professor, _, Courses),
    member(Course, Courses).

% Valid time slot within available time slots
valid_time_slot(Day, Start, End) :-
    available_time_slot(Day, AvailableStart, AvailableEnd),
    Start >= AvailableStart,
    End =< AvailableEnd.

% Find possible schedules for selected courses
find_schedule_for_courses(Courses, Schedule) :-
    find_schedule_for_courses_helper(Courses, [], Schedule).

find_schedule_for_courses_helper([], Acc, Acc).
find_schedule_for_courses_helper([Course | Rest], Acc, Schedule) :-
    course(Course, RoomType, _, _),
    can_teach(Professor, Course),
    available_professor(Professor, Day, Start, End),
    valid_time_slot(Day, Start, End),
    available_room(Room, RoomType),
    \+ conflict(Course, Professor, Room, Day, Start, End, Acc),
    NewAcc = [[Course, Professor, Room, Day, Start, End] | Acc],
    find_schedule_for_courses_helper(Rest, NewAcc, Schedule).

% Conflict rules
conflict(_, Professor, _, Day, Start1, End1, Schedule) :-
    member([_, Professor, _, Day, Start2, End2], Schedule),
    overlap(Start1, End1, Start2, End2).

conflict(_, _, Room, Day, Start1, End1, Schedule) :-
    member([_, _, Room, Day, Start2, End2], Schedule),
    overlap(Start1, End1, Start2, End2).

% Overlap definition
overlap(Start1, End1, Start2, End2) :-
    Start1 < End2,
    Start2 < End1.

% Encontrar horarios para cursos en semestres pares o impares
find_schedule_for_semester_parity(Parity, Schedule) :-
    % Obtener cursos que coinciden con la paridad
    findall(CourseName,
        (course(CourseName, _, _, Semester),
         ( (Parity = even, is_even(Semester)) ;
           (Parity = odd, is_odd(Semester)) )),
        Courses),
    % Encontrar horarios para esos cursos
    find_schedule_for_courses(Courses, Schedule).
