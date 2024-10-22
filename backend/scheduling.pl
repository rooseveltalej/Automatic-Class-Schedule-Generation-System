% Declarar predicados dinámicos para permitir la inserción de hechos en tiempo de ejecución
:- dynamic professor/3.
:- dynamic course/4.
:- dynamic course/5.

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

% Aleatorizar la selección de horarios para profesores
available_professor(Professor, Day, Start, End) :-
    professor(Professor, AvailableHours, _),
    findall([Day, Start, End], member([Day, Start, End], AvailableHours), Options),
    random_member([Day, Start, End], Options).

% Aleatorizar la selección de salones
available_room(Room, RoomType) :-
    findall(Room, room(Room, _, RoomType), Rooms),
    random_member(Room, Rooms).

% Modificar esta regla para que el intervalo del profesor esté dentro del intervalo disponible
valid_time_slot(Day, Start, End) :-
    available_time_slot(Day, AvailableStart, AvailableEnd),
    Start >= AvailableStart,
    End =< AvailableEnd.

% Definir si el profesor puede enseñar un curso
can_teach(Professor, Course) :-
    professor(Professor, _, Courses),
    member(Course, Courses).   

% Encontrar horarios posibles para los cursos seleccionados
find_schedule_for_courses(Courses, Schedule) :-
    find_schedule_for_courses_helper(Courses, [], Schedule).

find_schedule_for_courses_helper([], Acc, Acc).
find_schedule_for_courses_helper([Course | Rest], Acc, Schedule) :-
    course(Course, _, _, _),  % Usar course/4 ya que solo necesitamos 4 parámetros aquí
    can_teach(Professor, Course),
    available_professor(Professor, Day, Start, End),
    available_room(Room, RoomType),
    valid_time_slot(Day, Start, End),
    \+ conflict(Course, Professor, Room, Day, Start, End, Acc),
    NewAcc = [[Course, Professor, Room, Day, Start, End] | Acc],
    find_schedule_for_courses_helper(Rest, NewAcc, Schedule).

% Definir un predicado para evitar conflictos
conflict(_, Professor, _, Day, Start, End, Schedule) :-
    member([_, Professor, _, Day, Start, End], Schedule).

conflict(_, _, Room, Day, Start, End, Schedule) :-
    member([_, _, Room, Day, Start, End], Schedule).

conflict(_, _, _, Day, Start1, End1, Schedule) :-
    member([_, _, _, Day, Start2, End2], Schedule),
    overlap(Start1, End1, Start2, End2).

% Definir cómo determinar si dos intervalos de tiempo se solapan
overlap(Start1, End1, Start2, End2) :-
    Start1 < End2,
    Start2 < End1.

% Determinar la paridad de un curso basado en el semestre
parity_of_semester(Semester, even) :-
    Semester mod 2 =:= 0.  % Si es divisible por 2, es par
parity_of_semester(Semester, odd) :-
    Semester mod 2 =\= 0.  % Si no es divisible por 2, es impar

% Filtrar cursos según la paridad del semestre
find_schedule_for_semester_parity(Parity, Schedule) :-
    findall(Course, 
        (course(Course, _, _, Semester), parity_of_semester(Semester, Parity)),  % Filtrar por paridad
        Courses),
    find_schedule_for_courses(Courses, Schedule).
