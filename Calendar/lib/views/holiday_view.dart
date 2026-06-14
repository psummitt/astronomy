import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:provider/provider.dart';
import '../providers/calendar_provider.dart';
import '../models/holiday.dart';
import '../utils/calendar_engine.dart';

class HolidayView extends StatefulWidget {
  const HolidayView({super.key});

  @override
  State<HolidayView> createState() => _HolidayViewState();
}

class _HolidayViewState extends State<HolidayView> {
  late TextEditingController _yearController;
  String? _yearError;

  @override
  void initState() {
    super.initState();
    final provider = Provider.of<CalendarProvider>(context, listen: false);
    _yearController = TextEditingController(text: provider.selectedYear.toString());
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
    }
  }

  void _showAddHolidayDialog(BuildContext context, CalendarProvider provider) {
    final formKey = GlobalKey<FormState>();
    String name = "";
    int month = 1;
    int day = 1;
    bool isRecurring = true;

    showDialog(
      context: context,
      builder: (context) {
        return StatefulBuilder(
          builder: (context, setDialogState) {
            return AlertDialog(
              backgroundColor: const Color(0xFF161E31),
              title: const Text(
                'Add Custom Holiday',
                style: TextStyle(
                  fontFamily: 'Outfit',
                  color: Colors.white,
                  fontWeight: FontWeight.bold,
                ),
              ),
              content: Form(
                key: formKey,
                child: SingleChildScrollView(
                  child: Column(
                    mainAxisSize: MainAxisSize.min,
                    children: [
                      // Holiday name
                      TextFormField(
                        decoration: const InputDecoration(
                          labelText: 'Holiday Name',
                        ),
                        style: const TextStyle(fontFamily: 'Outfit'),
                        validator: (val) {
                          if (val == null || val.trim().isEmpty) return 'Enter a name';
                          return null;
                        },
                        onChanged: (val) => name = val,
                      ),
                      const SizedBox(height: 16),

                      // Month selection
                      DropdownButtonFormField<int>(
                        initialValue: month,
                        decoration: const InputDecoration(
                          labelText: 'Month',
                        ),
                        dropdownColor: const Color(0xFF161E31),
                        style: const TextStyle(
                          fontFamily: 'Outfit',
                          color: Colors.white,
                        ),
                        items: List.generate(12, (index) {
                          final m = index + 1;
                          return DropdownMenuItem(
                            value: m,
                            child: Text(CalendarEngine.monthNames[m].substring(0, 1) +
                                CalendarEngine.monthNames[m].substring(1).toLowerCase()),
                          );
                        }),
                        onChanged: (val) {
                          if (val != null) {
                            setDialogState(() => month = val);
                          }
                        },
                      ),
                      const SizedBox(height: 16),

                      // Day selection
                      TextFormField(
                        keyboardType: TextInputType.number,
                        inputFormatters: [FilteringTextInputFormatter.digitsOnly],
                        decoration: const InputDecoration(
                          labelText: 'Day of Month',
                        ),
                        style: const TextStyle(fontFamily: 'Outfit'),
                        validator: (val) {
                          if (val == null || val.isEmpty) return 'Enter day';
                          final d = int.tryParse(val);
                          final maxD = CalendarEngine.getDaysInMonth(provider.selectedYear, month);
                          if (d == null || d < 1 || d > maxD) {
                            return 'Enter 1-$maxD';
                          }
                          return null;
                        },
                        onChanged: (val) {
                          final d = int.tryParse(val);
                          if (d != null) day = d;
                        },
                      ),
                      const SizedBox(height: 16),

                      // Recurring vs Specific Year
                      SwitchListTile(
                        title: const Text(
                          'Repeats Annually',
                          style: TextStyle(
                            fontFamily: 'Outfit',
                            color: Colors.white70,
                            fontSize: 14,
                          ),
                        ),
                        value: isRecurring,
                        activeThumbColor: const Color(0xFF00F2FE),
                        onChanged: (val) {
                          setDialogState(() => isRecurring = val);
                        },
                      ),
                    ],
                  ),
                ),
              ),
              actions: [
                TextButton(
                  onPressed: () => Navigator.of(context).pop(),
                  child: const Text(
                    'Cancel',
                    style: TextStyle(color: Colors.white70),
                  ),
                ),
                ElevatedButton(
                  onPressed: () {
                    if (formKey.currentState!.validate()) {
                      final newH = Holiday(
                        id: 'cust_${DateTime.now().millisecondsSinceEpoch}',
                        name: name,
                        month: month,
                        day: day,
                        year: isRecurring ? null : provider.selectedYear,
                        isCustom: true,
                        isEnabled: true,
                      );
                      provider.addCustomHoliday(newH);
                      Navigator.of(context).pop();
                    }
                  },
                  child: const Text('Add'),
                ),
              ],
            );
          },
        );
      },
    );
  }

  @override
  Widget build(BuildContext context) {
    final provider = Provider.of<CalendarProvider>(context);
    final holidays = provider.getHolidaysForSelectedYear();

    return SingleChildScrollView(
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          const SizedBox(height: 16),
          // Header Input controls
          _buildHeaderControls(context, provider),
          const SizedBox(height: 16),

          // Holidays timeline/list
          Container(
            padding: const EdgeInsets.all(16),
            decoration: BoxDecoration(
              color: const Color(0xFF161E31),
              border: Border.all(
                color: Colors.white10,
                width: 1.0,
              ),
              borderRadius: BorderRadius.circular(16),
            ),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              children: [
                Text(
                  'Holidays Calendar - ${provider.selectedYear}',
                  style: const TextStyle(
                    fontFamily: 'Outfit',
                    fontSize: 18,
                    fontWeight: FontWeight.bold,
                    color: Colors.white,
                  ),
                ),
                const Divider(height: 24),
                holidays.isEmpty
                    ? const Center(
                        child: Padding(
                          padding: EdgeInsets.symmetric(vertical: 40.0),
                          child: Text(
                            'No holidays recorded for this year.',
                            style: TextStyle(
                              fontFamily: 'Outfit',
                              color: Color(0xFF64748B),
                            ),
                          ),
                        ),
                      )
                    : ListView.builder(
                        shrinkWrap: true,
                        physics: const NeverScrollableScrollPhysics(),
                        itemCount: holidays.length,
                        itemBuilder: (context, index) {
                          final h = holidays[index];
                          return _buildModernHolidayCard(provider, h);
                        },
                      ),
              ],
            ),
          ),
          const SizedBox(height: 16),
        ],
      ),
    );
  }

  Widget _buildHeaderControls(BuildContext context, CalendarProvider provider) {
    return Row(
      children: [
        // Year input
        Expanded(
          child: TextField(
            controller: _yearController,
            keyboardType: TextInputType.number,
            inputFormatters: [FilteringTextInputFormatter.digitsOnly],
            decoration: InputDecoration(
              labelText: 'Holiday Year',
              errorText: _yearError,
              prefixIcon: const Icon(Icons.calendar_month),
            ),
            style: const TextStyle(
              fontFamily: 'Outfit',
              fontSize: 16,
              fontWeight: FontWeight.bold,
            ),
            onChanged: (val) => _onYearChanged(val, provider),
          ),
        ),
        const SizedBox(width: 16),
        // Add custom holiday button
        ElevatedButton.icon(
          icon: const Icon(Icons.add),
          label: const Text(
            'Add Custom',
            style: TextStyle(fontFamily: 'Outfit'),
          ),
          onPressed: () => _showAddHolidayDialog(context, provider),
        ),
      ],
    );
  }

  Widget _buildModernHolidayCard(CalendarProvider provider, Holiday h) {
    final date = DateTime(provider.selectedYear, h.month, h.day);
    final weekdayName = CalendarEngine.weekdayNames[date.weekday - 1];
    final shortMonth = CalendarEngine.shortMonthNames[h.month];

    return Card(
      color: const Color(0xFF1E293B).withValues(alpha: 0.5),
      margin: const EdgeInsets.symmetric(vertical: 6.0),
      shape: RoundedRectangleBorder(
        borderRadius: BorderRadius.circular(12),
        side: BorderSide(
          color: h.isCustom 
            ? const Color(0xFF8B5CF6).withValues(alpha: 0.4)
            : Colors.white10,
        ),
      ),
      child: Padding(
        padding: const EdgeInsets.all(12.0),
        child: Row(
          children: [
            // Calendar icon / date tag
            Container(
              padding: const EdgeInsets.symmetric(horizontal: 10, vertical: 8),
              decoration: BoxDecoration(
                color: h.isEnabled 
                  ? (h.isCustom ? const Color(0xFF8B5CF6).withValues(alpha: 0.2) : const Color(0xFF4FACFE).withValues(alpha: 0.2))
                  : Colors.grey.withValues(alpha: 0.1),
                borderRadius: BorderRadius.circular(8),
              ),
              child: Column(
                children: [
                  Text(
                    shortMonth,
                    style: TextStyle(
                      fontWeight: FontWeight.bold,
                      fontSize: 12,
                      color: h.isEnabled 
                        ? (h.isCustom ? const Color(0xFFA78BFA) : const Color(0xFF38BDF8))
                        : Colors.grey,
                    ),
                  ),
                  Text(
                    h.day.toString(),
                    style: TextStyle(
                      fontWeight: FontWeight.w900,
                      fontSize: 18,
                      color: h.isEnabled ? Colors.white : Colors.grey,
                    ),
                  ),
                ],
              ),
            ),
            const SizedBox(width: 14),

            // Holiday Name
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    h.name,
                    style: TextStyle(
                      fontWeight: FontWeight.bold,
                      fontSize: 16,
                      color: h.isEnabled ? Colors.white : Colors.white38,
                      decoration: h.isEnabled ? null : TextDecoration.lineThrough,
                    ),
                  ),
                  const SizedBox(height: 4),
                  Row(
                    children: [
                      Text(
                        weekdayName.toLowerCase().substring(0, 1).toUpperCase() +
                            weekdayName.toLowerCase().substring(1),
                        style: const TextStyle(fontSize: 12, color: Color(0xFF64748B)),
                      ),
                      if (h.isCustom) ...[
                        const SizedBox(width: 8),
                        Container(
                          padding: const EdgeInsets.symmetric(horizontal: 6, vertical: 2),
                          decoration: BoxDecoration(
                            color: const Color(0xFF8B5CF6).withValues(alpha: 0.15),
                            borderRadius: BorderRadius.circular(4),
                          ),
                          child: const Text(
                            'CUSTOM',
                            style: TextStyle(fontSize: 9, color: Color(0xFFA78BFA), fontWeight: FontWeight.bold),
                          ),
                        ),
                      ],
                      if (h.isCustom && h.year == null) ...[
                        const SizedBox(width: 6),
                        Container(
                          padding: const EdgeInsets.symmetric(horizontal: 6, vertical: 2),
                          decoration: BoxDecoration(
                            color: Colors.white10,
                            borderRadius: BorderRadius.circular(4),
                          ),
                          child: const Text(
                            'RECURRING',
                            style: TextStyle(fontSize: 9, color: Colors.white60, fontWeight: FontWeight.bold),
                          ),
                        ),
                      ],
                    ],
                  ),
                ],
              ),
            ),

            // Controls (Enable Toggle & Delete)
            Switch(
              value: h.isEnabled,
              activeThumbColor: h.isCustom ? const Color(0xFF8B5CF6) : const Color(0xFF00F2FE),
              onChanged: (val) {
                provider.toggleHolidayEnabled(h.id, val);
              },
            ),
            if (h.isCustom)
              IconButton(
                icon: const Icon(Icons.delete, color: Colors.redAccent, size: 20),
                onPressed: () => provider.deleteCustomHoliday(h.id),
              ),
          ],
        ),
      ),
    );
  }
}
