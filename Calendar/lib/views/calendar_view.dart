import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:provider/provider.dart';
import '../providers/calendar_provider.dart';
import '../utils/calendar_engine.dart';
import '../models/holiday.dart';

class CalendarView extends StatefulWidget {
  const CalendarView({super.key});

  @override
  State<CalendarView> createState() => _CalendarViewState();
}

class _CalendarViewState extends State<CalendarView> {
  late TextEditingController _yearController;
  DateTime? _selectedDay;
  String? _yearError;

  @override
  void initState() {
    super.initState();
    final provider = Provider.of<CalendarProvider>(context, listen: false);
    _yearController = TextEditingController(text: provider.selectedYear.toString());
    _selectedDay = DateTime(provider.selectedYear, provider.selectedMonth, DateTime.now().day);
    // Bounds check day
    final maxDays = CalendarEngine.getDaysInMonth(provider.selectedYear, provider.selectedMonth);
    if (_selectedDay!.day > maxDays) {
      _selectedDay = DateTime(provider.selectedYear, provider.selectedMonth, 1);
    }
  }

  @override
  void dispose() {
    _yearController.dispose();
    super.dispose();
  }

  void _onYearChanged(String value, CalendarProvider provider) {
    if (value.isEmpty) return;
    final year = int.tryParse(value);
    if (year == null || year <= 1752) {
      setState(() {
        _yearError = "Year must be > 1752";
      });
    } else {
      setState(() {
        _yearError = null;
      });
      provider.setYear(year);
      // Keep selected day within bounds of new year
      _updateSelectedDayBounds(year, provider.selectedMonth);
    }
  }

  void _onMonthChanged(int month, CalendarProvider provider) {
    provider.setMonth(month);
    _updateSelectedDayBounds(provider.selectedYear, month);
  }

  void _updateSelectedDayBounds(int year, int month) {
    final maxDays = CalendarEngine.getDaysInMonth(year, month);
    int day = _selectedDay?.day ?? 1;
    if (day > maxDays) day = 1;
    setState(() {
      _selectedDay = DateTime(year, month, day);
    });
  }

  @override
  Widget build(BuildContext context) {
    final provider = Provider.of<CalendarProvider>(context);
    final isRetro = provider.themeMode == 'retro';

    final year = provider.selectedYear;
    final month = provider.selectedMonth;

    // Grid details
    final firstDayOfMonth = DateTime(year, month, 1);
    // Sunday = 0, Monday = 1, ... Saturday = 6
    final startOffset = firstDayOfMonth.weekday % 7;
    final totalDays = CalendarEngine.getDaysInMonth(year, month);

    final holidaysMap = provider.getHolidaysForSelectedMonth();

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        // Top Selection Area
        _buildSelectionHeader(context, provider, isRetro),
        const SizedBox(height: 16),

        // Main Grid and Day Details Side-by-Side on wide layouts, otherwise stacked
        Expanded(
          child: LayoutBuilder(
            builder: (context, constraints) {
              final isWide = constraints.maxWidth >= 600;

              final calendarGridWidget = _buildCalendarGrid(
                context,
                startOffset,
                totalDays,
                holidaysMap,
                isRetro,
              );

              final dayDetailsWidget = _buildDayDetails(isRetro);

              if (isWide) {
                return Row(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Expanded(flex: 3, child: calendarGridWidget),
                    const SizedBox(width: 20),
                    Expanded(flex: 2, child: dayDetailsWidget),
                  ],
                );
              } else {
                return SingleChildScrollView(
                  child: Column(
                    children: [
                      calendarGridWidget,
                      const SizedBox(height: 16),
                      dayDetailsWidget,
                    ],
                  ),
                );
              }
            },
          ),
        ),
      ],
    );
  }

  Widget _buildSelectionHeader(
      BuildContext context, CalendarProvider provider, bool isRetro) {
    return Container(
      padding: const EdgeInsets.all(12),
      decoration: BoxDecoration(
        color: isRetro ? Colors.transparent : const Color(0xFF161E31),
        border: Border.all(
          color: isRetro ? const Color(0xFF00AA00) : Colors.white10,
          width: isRetro ? 1.5 : 1.0,
        ),
        borderRadius: isRetro ? null : BorderRadius.circular(16),
      ),
      child: Row(
        children: [
          // Year text field
          Expanded(
            child: TextField(
              controller: _yearController,
              keyboardType: TextInputType.number,
              inputFormatters: [FilteringTextInputFormatter.digitsOnly],
              decoration: InputDecoration(
                labelText: isRetro ? 'YEAR' : 'Year',
                errorText: _yearError,
                prefixIcon: const Icon(Icons.calendar_today_outlined),
                border: isRetro ? const OutlineInputBorder() : null,
              ),
              style: TextStyle(
                fontFamily: isRetro ? 'monospace' : 'Outfit',
                fontSize: isRetro ? 16 : 18,
                fontWeight: FontWeight.bold,
              ),
              onChanged: (val) => _onYearChanged(val, provider),
            ),
          ),
          const SizedBox(width: 16),
          // Month drop down
          Expanded(
            child: DropdownButtonFormField<int>(
              initialValue: provider.selectedMonth,
              decoration: InputDecoration(
                labelText: isRetro ? 'MONTH' : 'Month',
                prefixIcon: const Icon(Icons.date_range),
                border: isRetro ? const OutlineInputBorder() : null,
              ),
              dropdownColor: isRetro ? const Color(0xFF020902) : const Color(0xFF161E31),
              style: TextStyle(
                fontFamily: isRetro ? 'monospace' : 'Outfit',
                fontSize: isRetro ? 16 : 18,
                color: isRetro ? const Color(0xFF33FF33) : Colors.white,
                fontWeight: FontWeight.bold,
              ),
              items: List.generate(12, (index) {
                final m = index + 1;
                return DropdownMenuItem(
                  value: m,
                  child: Text(
                    isRetro
                        ? CalendarEngine.monthNames[m]
                        : CalendarEngine.monthNames[m].substring(0, 1) +
                            CalendarEngine.monthNames[m].substring(1).toLowerCase(),
                  ),
                );
              }),
              onChanged: (val) {
                if (val != null) _onMonthChanged(val, provider);
              },
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildCalendarGrid(
      BuildContext context,
      int startOffset,
      int totalDays,
      Map<int, List<Holiday>> holidaysMap,
      bool isRetro) {
    // Weekday headers
    final headers = CalendarEngine.shortWeekdayLabels;

    // Days items list
    List<Widget> gridItems = [];

    // Header cells
    for (var h in headers) {
      gridItems.add(
        Center(
          child: Text(
            h,
            style: TextStyle(
              fontWeight: FontWeight.bold,
              color: isRetro 
                ? const Color(0xFF33FF33)
                : const Color(0xFF4FACFE),
              fontFamily: isRetro ? 'monospace' : 'Outfit',
              fontSize: 16,
            ),
          ),
        ),
      );
    }

    // Offset cells (empty)
    for (int i = 0; i < startOffset; i++) {
      gridItems.add(const SizedBox.shrink());
    }

    // Days cells
    for (int day = 1; day <= totalDays; day++) {
      final isSelected = _selectedDay?.day == day &&
          _selectedDay?.month == providerYearMonthMatch(context);
      
      final dayHolidays = holidaysMap[day] ?? [];
      final hasHoliday = dayHolidays.isNotEmpty;

      gridItems.add(
        GestureDetector(
          onTap: () {
            setState(() {
              final provider = Provider.of<CalendarProvider>(context, listen: false);
              _selectedDay = DateTime(provider.selectedYear, provider.selectedMonth, day);
            });
          },
          child: Container(
            margin: const EdgeInsets.all(4),
            decoration: BoxDecoration(
              color: isRetro
                  ? (isSelected ? const Color(0xFF004400) : Colors.transparent)
                  : (isSelected 
                      ? const Color(0xFF00F2FE).withValues(alpha: 0.25)
                      : (hasHoliday ? const Color(0xFF8B5CF6).withValues(alpha: 0.1) : const Color(0xFF1E293B).withValues(alpha: 0.3))),
              border: Border.all(
                color: isRetro
                    ? (isSelected
                        ? const Color(0xFF33FF33)
                        : (hasHoliday ? const Color(0xFF00AA00) : Colors.transparent))
                    : (isSelected
                        ? const Color(0xFF00F2FE)
                        : (hasHoliday ? const Color(0xFF8B5CF6).withValues(alpha: 0.5) : Colors.white10)),
                width: isRetro ? (isSelected ? 2.0 : 1.0) : 1.5,
              ),
              borderRadius: isRetro ? null : BorderRadius.circular(10),
            ),
            child: Stack(
              alignment: Alignment.center,
              children: [
                // Day number
                Text(
                  day.toString(),
                  style: TextStyle(
                    fontSize: 16,
                    fontWeight: isSelected || hasHoliday ? FontWeight.bold : FontWeight.normal,
                    color: isRetro
                        ? const Color(0xFF33FF33)
                        : (isSelected 
                            ? const Color(0xFF00F2FE) 
                            : (hasHoliday ? const Color(0xFFA78BFA) : Colors.white)),
                  ),
                ),
                // Indicator for holiday
                if (hasHoliday)
                  Positioned(
                    bottom: 4,
                    child: Container(
                      width: 5,
                      height: 5,
                      decoration: BoxDecoration(
                        color: isRetro ? const Color(0xFF33FF33) : const Color(0xFF8B5CF6),
                        shape: BoxShape.circle,
                      ),
                    ),
                  ),
              ],
            ),
          ),
        ),
      );
    }

    return Container(
      padding: const EdgeInsets.all(12),
      decoration: BoxDecoration(
        color: isRetro ? Colors.transparent : const Color(0xFF161E31),
        border: Border.all(
          color: isRetro ? const Color(0xFF00AA00) : Colors.white10,
          width: isRetro ? 1.5 : 1.0,
        ),
        borderRadius: isRetro ? null : BorderRadius.circular(16),
      ),
      child: GridView.count(
        shrinkWrap: true,
        physics: const NeverScrollableScrollPhysics(),
        crossAxisCount: 7,
        childAspectRatio: 1.1,
        children: gridItems,
      ),
    );
  }

  int providerYearMonthMatch(BuildContext context) {
    return Provider.of<CalendarProvider>(context, listen: false).selectedMonth;
  }

  Widget _buildDayDetails(bool isRetro) {
    if (_selectedDay == null) {
      return Container(
        height: 200,
        alignment: Alignment.center,
        child: Text(
          isRetro ? 'SELECT A DAY' : 'Select a day to view details',
          style: TextStyle(fontFamily: isRetro ? 'monospace' : 'Outfit'),
        ),
      );
    }

    final provider = Provider.of<CalendarProvider>(context);
    final holidaysMap = provider.getHolidaysForSelectedMonth();
    final dayHolidays = holidaysMap[_selectedDay!.day] ?? [];

    final dayOfYear = CalendarEngine.getDayOfYear(_selectedDay!);
    final daysRemaining = CalendarEngine.getDaysRemainingInYear(_selectedDay!);
    final weekdayName = CalendarEngine.weekdayNames[_selectedDay!.weekday - 1];

    return Container(
      width: double.infinity,
      padding: const EdgeInsets.all(16),
      decoration: BoxDecoration(
        color: isRetro ? Colors.transparent : const Color(0xFF161E31),
        border: Border.all(
          color: isRetro ? const Color(0xFF00AA00) : Colors.white10,
          width: isRetro ? 1.5 : 1.0,
        ),
        borderRadius: isRetro ? null : BorderRadius.circular(16),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          // Day Header
          Text(
            isRetro ? '--- DAY REPORT ---' : 'DAY DETAILS',
            style: TextStyle(
              fontSize: 14,
              fontWeight: FontWeight.bold,
              color: isRetro ? const Color(0xFF33FF33) : const Color(0xFF00F2FE),
              letterSpacing: 1.2,
            ),
          ),
          const SizedBox(height: 12),
          Text(
            '${_selectedDay!.day} ${CalendarEngine.shortMonthNames[_selectedDay!.month]} ${_selectedDay!.year}',
            style: TextStyle(
              fontSize: 22,
              fontWeight: FontWeight.bold,
              fontFamily: isRetro ? 'monospace' : 'Outfit',
              color: isRetro ? const Color(0xFF33FF33) : Colors.white,
            ),
          ),
          Text(
            weekdayName,
            style: TextStyle(
              fontSize: 14,
              fontFamily: isRetro ? 'monospace' : 'Outfit',
              color: isRetro ? const Color(0xFF00AA00) : const Color(0xFF94A3B8),
            ),
          ),
          const Divider(height: 24),

          // Day stats (Applesoft inspired)
          _buildInfoRow(isRetro, 'Day of Year', '$dayOfYear'),
          _buildInfoRow(isRetro, 'Days Remaining', '$daysRemaining'),
          
          const Divider(height: 24),

          // Holidays today
          Text(
            isRetro ? 'HOLIDAYS:' : 'Holidays on this day:',
            style: TextStyle(
              fontSize: 14,
              fontWeight: FontWeight.bold,
              fontFamily: isRetro ? 'monospace' : 'Outfit',
              color: isRetro ? const Color(0xFF33FF33) : Colors.white,
            ),
          ),
          const SizedBox(height: 8),
          if (dayHolidays.isEmpty)
            Text(
              isRetro ? '[NONE]' : 'No holidays',
              style: TextStyle(
                fontFamily: isRetro ? 'monospace' : 'Outfit',
                color: isRetro ? const Color(0xFF00AA00) : const Color(0xFF64748B),
              ),
            )
          else
            ...dayHolidays.map(
              (h) => Padding(
                padding: const EdgeInsets.symmetric(vertical: 4.0),
                child: Row(
                  children: [
                    Icon(
                      Icons.star,
                      size: 16,
                      color: isRetro ? const Color(0xFF33FF33) : const Color(0xFF8B5CF6),
                    ),
                    const SizedBox(width: 8),
                    Expanded(
                      child: Text(
                        h.name,
                        style: TextStyle(
                          fontFamily: isRetro ? 'monospace' : 'Outfit',
                          fontWeight: FontWeight.bold,
                          color: isRetro ? const Color(0xFF33FF33) : Colors.white,
                        ),
                      ),
                    ),
                  ],
                ),
              ),
            ),
        ],
      ),
    );
  }

  Widget _buildInfoRow(bool isRetro, String label, String value) {
    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 6.0),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.spaceBetween,
        children: [
          Expanded(
            child: Text(
              isRetro ? '${label.toUpperCase()}:' : label,
              style: TextStyle(
                fontFamily: isRetro ? 'monospace' : 'Outfit',
                color: isRetro ? const Color(0xFF00AA00) : const Color(0xFF94A3B8),
              ),
              overflow: TextOverflow.ellipsis,
            ),
          ),
          const SizedBox(width: 8),
          Text(
            value,
            style: TextStyle(
              fontWeight: FontWeight.bold,
              fontFamily: isRetro ? 'monospace' : 'Outfit',
              color: isRetro ? const Color(0xFF33FF33) : Colors.white,
            ),
          ),
        ],
      ),
    );
  }
}
