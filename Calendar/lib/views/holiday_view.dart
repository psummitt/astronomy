import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:provider/provider.dart';
import '../providers/calendar_provider.dart';
import '../models/holiday.dart';
import '../utils/calendar_engine.dart';

class HolidayView extends StatefulWidget {
  const HolidayView({Key? key}) : super(key: key);

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

  void _showAddHolidayDialog(BuildContext context, CalendarProvider provider, bool isRetro) {
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
              backgroundColor: isRetro ? const Color(0xFF020902) : const Color(0xFF161E31),
              title: Text(
                isRetro ? 'ADD CUSTOM HOLIDAY' : 'Add Custom Holiday',
                style: TextStyle(
                  fontFamily: isRetro ? 'monospace' : 'Outfit',
                  color: isRetro ? const Color(0xFF33FF33) : Colors.white,
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
                        decoration: InputDecoration(
                          labelText: isRetro ? 'NAME' : 'Holiday Name',
                          border: isRetro ? const OutlineInputBorder() : null,
                        ),
                        style: TextStyle(fontFamily: isRetro ? 'monospace' : 'Outfit'),
                        validator: (val) {
                          if (val == null || val.trim().isEmpty) return 'Enter a name';
                          return null;
                        },
                        onChanged: (val) => name = val,
                      ),
                      const SizedBox(height: 16),

                      // Month selection
                      DropdownButtonFormField<int>(
                        value: month,
                        decoration: InputDecoration(
                          labelText: isRetro ? 'MONTH' : 'Month',
                          border: isRetro ? const OutlineInputBorder() : null,
                        ),
                        dropdownColor: isRetro ? const Color(0xFF020902) : const Color(0xFF161E31),
                        style: TextStyle(
                          fontFamily: isRetro ? 'monospace' : 'Outfit',
                          color: isRetro ? const Color(0xFF33FF33) : Colors.white,
                        ),
                        items: List.generate(12, (index) {
                          final m = index + 1;
                          return DropdownMenuItem(
                            value: m,
                            child: Text(CalendarEngine.monthNames[m]),
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
                        decoration: InputDecoration(
                          labelText: isRetro ? 'DAY' : 'Day of Month',
                          border: isRetro ? const OutlineInputBorder() : null,
                        ),
                        style: TextStyle(fontFamily: isRetro ? 'monospace' : 'Outfit'),
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
                        title: Text(
                          isRetro ? 'RECURRING ANNUALLY' : 'Repeats Annually',
                          style: TextStyle(
                            fontFamily: isRetro ? 'monospace' : 'Outfit',
                            color: isRetro ? const Color(0xFF33FF33) : Colors.white70,
                            fontSize: 14,
                          ),
                        ),
                        value: isRecurring,
                        activeColor: isRetro ? const Color(0xFF33FF33) : const Color(0xFF00F2FE),
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
                  child: Text(
                    isRetro ? 'CANCEL' : 'Cancel',
                    style: TextStyle(color: isRetro ? const Color(0xFF00AA00) : Colors.white70),
                  ),
                ),
                ElevatedButton(
                  style: ElevatedButton.styleFrom(
                    shape: isRetro ? const BeveledRectangleBorder() : null,
                  ),
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
                  child: Text(isRetro ? 'ADD' : 'Add'),
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
    final isRetro = provider.themeMode == 'retro';
    final holidays = provider.getHolidaysForSelectedYear();

    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        const SizedBox(height: 16),
        // Header Input controls
        _buildHeaderControls(context, provider, isRetro),
        const SizedBox(height: 16),

        // Holidays timeline/list
        Expanded(
          child: Container(
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
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    Text(
                      isRetro 
                        ? 'FOR YEAR ${provider.selectedYear} THE HOLIDAYS ARE ...'
                        : 'Holidays Calendar - ${provider.selectedYear}',
                      style: TextStyle(
                        fontFamily: isRetro ? 'monospace' : 'Outfit',
                        fontSize: isRetro ? 14 : 18,
                        fontWeight: FontWeight.bold,
                        color: isRetro ? const Color(0xFF33FF33) : Colors.white,
                      ),
                    ),
                    if (isRetro)
                      Text(
                        'COUNT: ${holidays.length}',
                        style: const TextStyle(fontFamily: 'monospace', color: Color(0xFF00AA00)),
                      ),
                  ],
                ),
                const Divider(height: 24),
                Expanded(
                  child: holidays.isEmpty
                      ? Center(
                          child: Text(
                            isRetro ? '[NO HOLIDAYS FOUND]' : 'No holidays recorded for this year.',
                            style: TextStyle(
                              fontFamily: isRetro ? 'monospace' : 'Outfit',
                              color: isRetro ? const Color(0xFF00AA00) : const Color(0xFF64748B),
                            ),
                          ),
                        )
                      : ListView.builder(
                          itemCount: holidays.length,
                          itemBuilder: (context, index) {
                            final h = holidays[index];
                            return isRetro 
                              ? _buildRetroHolidayRow(provider, h) 
                              : _buildModernHolidayCard(provider, h);
                          },
                        ),
                ),
              ],
            ),
          ),
        ),
      ],
    );
  }

  Widget _buildHeaderControls(
      BuildContext context, CalendarProvider provider, bool isRetro) {
    return Row(
      children: [
        // Year input
        Expanded(
          child: TextField(
            controller: _yearController,
            keyboardType: TextInputType.number,
            inputFormatters: [FilteringTextInputFormatter.digitsOnly],
            decoration: InputDecoration(
              labelText: isRetro ? 'YEAR' : 'Holiday Year',
              errorText: _yearError,
              prefixIcon: const Icon(Icons.calendar_month),
              border: isRetro ? const OutlineInputBorder() : null,
            ),
            style: TextStyle(
              fontFamily: isRetro ? 'monospace' : 'Outfit',
              fontSize: 16,
              fontWeight: FontWeight.bold,
            ),
            onChanged: (val) => _onYearChanged(val, provider),
          ),
        ),
        const SizedBox(width: 16),
        // Add custom holiday button
        ElevatedButton.icon(
          style: ElevatedButton.styleFrom(
            shape: isRetro ? const BeveledRectangleBorder() : null,
          ),
          icon: const Icon(Icons.add),
          label: Text(
            isRetro ? 'ADD HOLIDAY' : 'Add Custom',
            style: TextStyle(fontFamily: isRetro ? 'monospace' : 'Outfit'),
          ),
          onPressed: () => _showAddHolidayDialog(context, provider, isRetro),
        ),
      ],
    );
  }

  Widget _buildRetroHolidayRow(CalendarProvider provider, Holiday h) {
    // Calculate weekday for the holiday date
    final date = DateTime(provider.selectedYear, h.month, h.day);
    final weekdayName = CalendarEngine.weekdayNames[date.weekday - 1];
    final shortMonth = CalendarEngine.shortMonthNames[h.month];
    
    final label = h.name.toUpperCase();
    final dateStr = '$weekdayName, $shortMonth. ${h.day}';

    // Build the dotted tab effect like early printer reports
    final totalWidthChars = 40;
    final dotCount = totalWidthChars - label.length - dateStr.length;
    final dots = dotCount > 0 ? '.' * dotCount : ' ';

    return Padding(
      padding: const EdgeInsets.symmetric(vertical: 4.0),
      child: Opacity(
        opacity: h.isEnabled ? 1.0 : 0.4,
        child: Row(
          children: [
            // Enable toggle checkbox
            Checkbox(
              value: h.isEnabled,
              activeColor: const Color(0xFF33FF33),
              checkColor: const Color(0xFF020902),
              side: const BorderSide(color: Color(0xFF33FF33)),
              onChanged: (val) {
                if (val != null) {
                  provider.toggleHolidayEnabled(h.id, val);
                }
              },
            ),
            Expanded(
              child: Text(
                '$label$dots$dateStr',
                style: TextStyle(
                  fontFamily: 'monospace',
                  fontSize: 13,
                  color: h.isCustom ? const Color(0xFF33FF33) : const Color(0xFF33FF33),
                  fontWeight: h.isCustom ? FontWeight.bold : FontWeight.normal,
                ),
              ),
            ),
            if (h.isCustom)
              IconButton(
                icon: const Icon(Icons.delete_forever, color: Color(0xFF33FF33), size: 18),
                padding: EdgeInsets.zero,
                constraints: const BoxConstraints(),
                onPressed: () => provider.deleteCustomHoliday(h.id),
              ),
          ],
        ),
      ),
    );
  }

  Widget _buildModernHolidayCard(CalendarProvider provider, Holiday h) {
    final date = DateTime(provider.selectedYear, h.month, h.day);
    final weekdayName = CalendarEngine.weekdayNames[date.weekday - 1];
    final shortMonth = CalendarEngine.shortMonthNames[h.month];

    return Card(
      color: const Color(0xFF1E293B).withOpacity(0.5),
      margin: const EdgeInsets.symmetric(vertical: 6.0),
      shape: RoundedRectangleBorder(
        borderRadius: BorderRadius.circular(12),
        side: BorderSide(
          color: h.isCustom 
            ? const Color(0xFF8B5CF6).withOpacity(0.4)
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
                  ? (h.isCustom ? const Color(0xFF8B5CF6).withOpacity(0.2) : const Color(0xFF4FACFE).withOpacity(0.2))
                  : Colors.grey.withOpacity(0.1),
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
                            color: const Color(0xFF8B5CF6).withOpacity(0.15),
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
              activeColor: h.isCustom ? const Color(0xFF8B5CF6) : const Color(0xFF00F2FE),
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
