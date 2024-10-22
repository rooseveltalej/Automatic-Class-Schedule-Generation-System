import React from 'react';
import { Calendar, momentLocalizer } from 'react-big-calendar';
import moment from 'moment';
import 'react-big-calendar/lib/css/react-big-calendar.css';

// Configurar moment como el localizador de fechas
const localizer = momentLocalizer(moment);

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

// Función para convertir los días y horas en formato Date de JS
const convertDayToDate = (day: string, time: number) => {
  const daysMap: { [key: string]: number } = {
    monday: 1,
    tuesday: 2,
    wednesday: 3,
    thursday: 4,
    friday: 5,
  };

  // Descomponer la hora en horas y minutos
  const hours = Math.floor(time); // Parte entera es la hora
  const minutes = (time % 1) * 60; // Parte decimal convertida a minutos
  
  const dayOffset = daysMap[day.toLowerCase()] || 1; // Default to Monday if not found
  return moment()
    .startOf('week') // Ir al principio de la semana (domingo)
    .add(dayOffset, 'days')
    .set('hour', hours)
    .set('minute', minutes)
    .toDate(); // Convertir a formato Date
};

// Función para ajustar el formato de la hora (para formato de 24 horas)
const adjustTo24HourFormat = (time: number) => {
  // Si la hora está entre 1 PM y 11 PM, convertir a formato de 24 horas
  if (time >= 1 && time <= 4) {
    return time + 12; // Convertir 1 PM a 13, 2 PM a 14, etc.
  }
  // Si la hora es entre 7 AM y 11 AM, no se modifica
  return time;
};

// Función para formatear los textos y hacerlos más legibles (eliminar _ y capitalizar palabras)
const formatText = (text: string) => {
  return text
    .split('_')
    .map((word) => word.charAt(0).toUpperCase() + word.slice(1))
    .join(' ');
};

const ScheduleDisplay: React.FC<ScheduleDisplayProps> = ({ schedule }) => {
  return (
    <div className="space-y-8">
      {schedule.map((option, index) => {
        // Convertir los datos del horario en eventos para cada opción de calendario
        const events = option.map((item) => {
          const adjustedStart = adjustTo24HourFormat(item.start); // Ajustar solo las horas de la tarde
          const adjustedEnd = adjustTo24HourFormat(item.end); // Ajustar solo las horas de la tarde
          return {
            title: `${formatText(item.course)} (${formatText(item.professor)})`,
            start: convertDayToDate(item.day, adjustedStart),
            end: convertDayToDate(item.day, adjustedEnd),
            description: item.room,
          };
        });

        return (
          <div key={index}>
            <h3 className="text-xl font-semibold mb-4">Option {index + 1}</h3>
            <div className="calendar-container" style={{ height: '500px', marginBottom: '20px' }}>
              <Calendar
                localizer={localizer}
                events={events}
                defaultView="week"
                startAccessor="start"
                endAccessor="end"
                style={{ height: 500 }}
                eventPropGetter={(event) => ({
                  style: {
                    backgroundColor: '#3182ce', // Color de fondo para los eventos
                    color: 'white',
                    borderRadius: '5px',
                    padding: '5px',
                  },
                })}
              />
            </div>
          </div>
        );
      })}
    </div>
  );
};

export default ScheduleDisplay;
