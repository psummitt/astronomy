import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'package:intl/intl.dart';
import 'package:table_calendar/table_calendar.dart';

import '../../logbook/data/observation_repository.dart';
import '../../logbook/domain/observation_log.dart';
import '../../planner/data/plan_provider.dart';
import '../../planner/domain/planner_observation.dart';

class CalendarPage extends ConsumerStatefulWidget {
  const CalendarPage({super.key});

  @override
  ConsumerState<CalendarPage> createState() => _CalendarPageState();
}

class _CalendarPageState extends ConsumerState<CalendarPage> {
  CalendarFormat _calendarFormat = CalendarFormat.month;
  DateTime _focusedDay = DateTime.now();
  DateTime? _selectedDay;

  @override
  void initState() {
    super.initState();
    _selectedDay = _focusedDay;
  }

  bool _isSameDay(DateTime? a, DateTime? b) {
    if (a == null || b == null) return false;
    return a.year == b.year && a.month == b.month && a.day == b.day;
  }

  List<PlannerObservation> _getPlannedForDay(
      DateTime day, List<PlannerObservation> allPlans) {
    return allPlans.where((plan) => _isSameDay(plan.dateTime, day)).toList();
  }

  List<ObservationLog> _getObservedForDay(
      DateTime day, List<ObservationLog> allLogs) {
    return allLogs.where((log) => _isSameDay(log.observationDate, day)).toList();
  }

  @override
  Widget build(BuildContext context) {
    final plannedList = ref.watch(planProvider);
    final observedAsync = ref.watch(observationsStreamProvider);
    final observedList = observedAsync.value ?? [];

    final activeSelectedDay = _selectedDay ?? _focusedDay;
    final plansForSelectedDay = _getPlannedForDay(activeSelectedDay, plannedList);
    final logsForSelectedDay = _getObservedForDay(activeSelectedDay, observedList);

    return Scaffold(
      appBar: AppBar(
        title: const Text('AstroDiary Calendar'),
        actions: [
          Semantics(
            button: true,
            label: 'Jump to Today',
            child: IconButton(
              icon: const Icon(Icons.today),
              tooltip: 'Jump to Today',
              onPressed: () {
                setState(() {
                  _focusedDay = DateTime.now();
                  _selectedDay = _focusedDay;
                });
              },
            ),
          ),
        ],
      ),
      body: Column(
        children: [
          Semantics(
            label: 'Observation Calendar View',
            child: Card(
              margin: const EdgeInsets.all(8.0),
              child: TableCalendar(
                firstDay: DateTime.utc(2000, 1, 1),
                lastDay: DateTime.utc(2100, 12, 31),
                focusedDay: _focusedDay,
                calendarFormat: _calendarFormat,
                selectedDayPredicate: (day) => _isSameDay(_selectedDay, day),
                onDaySelected: (selectedDay, focusedDay) {
                  setState(() {
                    _selectedDay = selectedDay;
                    _focusedDay = focusedDay;
                  });
                },
                onFormatChanged: (format) {
                  setState(() {
                    _calendarFormat = format;
                  });
                },
                onPageChanged: (focusedDay) {
                  _focusedDay = focusedDay;
                },
                eventLoader: (day) {
                  final plans = _getPlannedForDay(day, plannedList);
                  final logs = _getObservedForDay(day, observedList);
                  List<String> events = [];
                  if (plans.isNotEmpty) events.add('plan');
                  if (logs.isNotEmpty) events.add('log');
                  return events;
                },
                calendarBuilders: CalendarBuilders(
                  markerBuilder: (context, date, events) {
                    if (events.isEmpty) return const SizedBox();
                    final plans = _getPlannedForDay(date, plannedList);
                    final logs = _getObservedForDay(date, observedList);

                    return Row(
                      mainAxisAlignment: MainAxisAlignment.center,
                      children: [
                        if (plans.isNotEmpty)
                          Container(
                            margin: const EdgeInsets.symmetric(horizontal: 1.5),
                            width: 7,
                            height: 7,
                            decoration: const BoxDecoration(
                              shape: BoxShape.circle,
                              color: Colors.blueAccent,
                            ),
                          ),
                        if (logs.isNotEmpty)
                          Container(
                            margin: const EdgeInsets.symmetric(horizontal: 1.5),
                            width: 7,
                            height: 7,
                            decoration: const BoxDecoration(
                              shape: BoxShape.circle,
                              color: Colors.greenAccent,
                            ),
                          ),
                      ],
                    );
                  },
                ),
              ),
            ),
          ),

          // Date Header
          Padding(
            padding: const EdgeInsets.symmetric(horizontal: 16.0, vertical: 8.0),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                Semantics(
                  header: true,
                  label: 'Selected Date: ${DateFormat('EEEE, MMM d, yyyy').format(activeSelectedDay)}',
                  child: Text(
                    DateFormat('EEEE, MMM d, yyyy').format(activeSelectedDay),
                    style: Theme.of(context).textTheme.titleMedium?.copyWith(
                          fontWeight: FontWeight.bold,
                        ),
                  ),
                ),
                Row(
                  children: [
                    Semantics(
                      button: true,
                      label: 'Add Target Plan for selected date',
                      child: ElevatedButton.icon(
                        style: ElevatedButton.styleFrom(
                          padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                        ),
                        onPressed: () {
                          context.go('/planner/add', extra: activeSelectedDay);
                        },
                        icon: const Icon(Icons.add_task, size: 18),
                        label: const Text('Add Plan'),
                      ),
                    ),
                    const SizedBox(width: 8),
                    Semantics(
                      button: true,
                      label: 'Add Observation Log for selected date',
                      child: OutlinedButton.icon(
                        style: OutlinedButton.styleFrom(
                          padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
                        ),
                        onPressed: () {
                          context.go('/logbook/add');
                        },
                        icon: const Icon(Icons.menu_book, size: 18),
                        label: const Text('Add Log'),
                      ),
                    ),
                  ],
                ),
              ],
            ),
          ),

          const Divider(height: 1),

          // Selected Day Observations List
          Expanded(
            child: (plansForSelectedDay.isEmpty && logsForSelectedDay.isEmpty)
                ? Center(
                    child: Column(
                      mainAxisAlignment: MainAxisAlignment.center,
                      children: [
                        Icon(
                          Icons.event_available,
                          size: 48,
                          color: Theme.of(context).colorScheme.primary.withValues(alpha: 0.5),
                        ),
                        const SizedBox(height: 12),
                        Text(
                          'No observations for this date',
                          style: Theme.of(context).textTheme.bodyLarge,
                        ),
                        const SizedBox(height: 4),
                        const Text(
                          'Tap "Add Plan" or "Add Log" to schedule or record an entry.',
                          style: TextStyle(color: Colors.grey, fontSize: 13),
                        ),
                      ],
                    ),
                  )
                : ListView(
                    padding: const EdgeInsets.all(12),
                    children: [
                      if (plansForSelectedDay.isNotEmpty) ...[
                        const Padding(
                          padding: EdgeInsets.symmetric(vertical: 6, horizontal: 4),
                          child: Row(
                            children: [
                              Icon(Icons.circle, color: Colors.blueAccent, size: 12),
                              SizedBox(width: 6),
                              Text('Planned Targets',
                                  style: TextStyle(fontWeight: FontWeight.bold, fontSize: 15)),
                            ],
                          ),
                        ),
                        ...plansForSelectedDay.map((plan) => Card(
                              child: ListTile(
                                title: Text(plan.targetName,
                                    style: const TextStyle(fontWeight: FontWeight.bold)),
                                subtitle: Text(
                                    '${DateFormat('HH:mm').format(plan.dateTime)} • RA: ${plan.ra} | Dec: ${plan.dec}\n${plan.notes}'),
                                isThreeLine: plan.notes.isNotEmpty,
                                trailing: Semantics(
                                  button: true,
                                  label: 'Delete plan ${plan.targetName}',
                                  child: IconButton(
                                    icon: const Icon(Icons.delete_outline, color: Colors.redAccent),
                                    onPressed: () {
                                      ref.read(planProvider.notifier).deleteObservation(plan.id);
                                    },
                                  ),
                                ),
                              ),
                            )),
                      ],
                      if (logsForSelectedDay.isNotEmpty) ...[
                        const SizedBox(height: 12),
                        const Padding(
                          padding: EdgeInsets.symmetric(vertical: 6, horizontal: 4),
                          child: Row(
                            children: [
                              Icon(Icons.circle, color: Colors.greenAccent, size: 12),
                              SizedBox(width: 6),
                              Text('Recorded Logs',
                                  style: TextStyle(fontWeight: FontWeight.bold, fontSize: 15)),
                            ],
                          ),
                        ),
                        ...logsForSelectedDay.map((log) => Card(
                              child: ListTile(
                                title: Text(log.target.name,
                                    style: const TextStyle(fontWeight: FontWeight.bold)),
                                subtitle: Text(
                                    '${DateFormat('HH:mm').format(log.startTime)} • Instrument: ${log.instrument.name}\n${log.notes}'),
                                isThreeLine: log.notes.isNotEmpty,
                                trailing: const Icon(Icons.chevron_right),
                                onTap: () {
                                  context.go('/logbook/detail/${log.id}', extra: log);
                                },
                              ),
                            )),
                      ],
                    ],
                  ),
          ),
        ],
      ),
    );
  }
}
