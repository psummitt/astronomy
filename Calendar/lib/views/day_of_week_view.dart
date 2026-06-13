import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:provider/provider.dart';
import '../providers/calendar_provider.dart';
import '../utils/calendar_engine.dart';

class DayOfWeekView extends StatefulWidget {
  const DayOfWeekView({super.key});

  @override
  State<DayOfWeekView> createState() => _DayOfWeekViewState();
}

class _DayOfWeekViewState extends State<DayOfWeekView> {
  final _formKey = GlobalKey<FormState>();
  late TextEditingController _yearController;
  late TextEditingController _monthController;
  late TextEditingController _dayController;

  String? _calculatedDay;
  int? _calcYear;
  int? _calcMonth;
  int? _calcDay;
  bool _hasCalculated = false;

  @override
  void initState() {
    super.initState();
    final now = DateTime.now();
    _yearController = TextEditingController(text: now.year.toString());
    _monthController = TextEditingController(text: now.month.toString());
    _dayController = TextEditingController(text: now.day.toString());
  }

  @override
  void dispose() {
    _yearController.dispose();
    _monthController.dispose();
    _dayController.dispose();
    super.dispose();
  }

  void _calculateDay() {
    if (!_formKey.currentState!.validate()) return;

    final y = int.parse(_yearController.text);
    final m = int.parse(_monthController.text);
    final d = int.parse(_dayController.text);

    // Date validation bounds
    final maxDays = CalendarEngine.getDaysInMonth(y, m);
    if (d > maxDays) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text('Invalid date: $m/$d/$y has only $maxDays days.'),
          backgroundColor: Colors.redAccent,
        ),
      );
      return;
    }

    final date = DateTime(y, m, d);
    final weekdayIndex = date.weekday - 1; // 0 = Monday, 6 = Sunday
    
    setState(() {
      _calcYear = y;
      _calcMonth = m;
      _calcDay = d;
      _calculatedDay = CalendarEngine.weekdayNames[weekdayIndex];
      _hasCalculated = true;
    });
  }

  @override
  Widget build(BuildContext context) {
    final isRetro = Provider.of<CalendarProvider>(context).themeMode == 'retro';

    return SingleChildScrollView(
      child: Form(
        key: _formKey,
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            const SizedBox(height: 16),
            Text(
              isRetro ? '--- DAY OF WEEK FINDER ---' : 'Day of Week Finder',
              style: TextStyle(
                fontSize: isRetro ? 18 : 22,
                fontWeight: FontWeight.bold,
                fontFamily: isRetro ? 'monospace' : 'Outfit',
                color: isRetro ? const Color(0xFF33FF33) : Colors.white,
              ),
            ),
            const SizedBox(height: 16),

            // Card inputs
            Container(
              padding: const EdgeInsets.all(20),
              decoration: BoxDecoration(
                color: isRetro ? Colors.transparent : const Color(0xFF161E31),
                border: Border.all(
                  color: isRetro ? const Color(0xFF00AA00) : Colors.white10,
                  width: isRetro ? 1.5 : 1.0,
                ),
                borderRadius: isRetro ? null : BorderRadius.circular(16),
              ),
              child: Column(
                children: [
                  // Year Field
                  TextFormField(
                    controller: _yearController,
                    keyboardType: TextInputType.number,
                    inputFormatters: [FilteringTextInputFormatter.digitsOnly],
                    decoration: InputDecoration(
                      labelText: isRetro ? 'THE YEAR' : 'Year',
                      prefixIcon: const Icon(Icons.calendar_today),
                      border: isRetro ? const OutlineInputBorder() : null,
                    ),
                    style: TextStyle(fontFamily: isRetro ? 'monospace' : 'Outfit', fontSize: 16),
                    validator: (val) {
                      if (val == null || val.isEmpty) return 'Enter Year';
                      final y = int.tryParse(val);
                      if (y == null || y <= 1752) return 'Year must be > 1752';
                      return null;
                    },
                  ),
                  const SizedBox(height: 16),
                  
                  // Month Field
                  TextFormField(
                    controller: _monthController,
                    keyboardType: TextInputType.number,
                    inputFormatters: [FilteringTextInputFormatter.digitsOnly],
                    decoration: InputDecoration(
                      labelText: isRetro ? 'THE MONTH' : 'Month (1-12)',
                      prefixIcon: const Icon(Icons.date_range),
                      border: isRetro ? const OutlineInputBorder() : null,
                    ),
                    style: TextStyle(fontFamily: isRetro ? 'monospace' : 'Outfit', fontSize: 16),
                    validator: (val) {
                      if (val == null || val.isEmpty) return 'Enter Month';
                      final m = int.tryParse(val);
                      if (m == null || m < 1 || m > 12) return 'Month must be 1-12';
                      return null;
                    },
                  ),
                  const SizedBox(height: 16),

                  // Day Field
                  TextFormField(
                    controller: _dayController,
                    keyboardType: TextInputType.number,
                    inputFormatters: [FilteringTextInputFormatter.digitsOnly],
                    decoration: InputDecoration(
                      labelText: isRetro ? 'THE DAY' : 'Day',
                      prefixIcon: const Icon(Icons.today),
                      border: isRetro ? const OutlineInputBorder() : null,
                    ),
                    style: TextStyle(fontFamily: isRetro ? 'monospace' : 'Outfit', fontSize: 16),
                    validator: (val) {
                      if (val == null || val.isEmpty) return 'Enter Day';
                      final d = int.tryParse(val);
                      if (d == null || d < 1 || d > 31) return 'Day must be 1-31';
                      return null;
                    },
                  ),
                  const SizedBox(height: 24),

                  // Calculate Button
                  SizedBox(
                    width: double.infinity,
                    child: ElevatedButton(
                      onPressed: _calculateDay,
                      style: ElevatedButton.styleFrom(
                        shape: isRetro ? const BeveledRectangleBorder() : null,
                      ),
                      child: Text(
                        isRetro ? 'RUN CALCULATION' : 'Find Day of Week',
                        style: const TextStyle(fontSize: 16),
                      ),
                    ),
                  ),
                ],
              ),
            ),
            const SizedBox(height: 24),

            // Result Display
            if (_hasCalculated)
              AnimatedOpacity(
                opacity: _hasCalculated ? 1.0 : 0.0,
                duration: const Duration(milliseconds: 300),
                child: isRetro 
                  ? _buildRetroResult()
                  : _buildModernResult(),
              ),
          ],
        ),
      ),
    );
  }

  Widget _buildRetroResult() {
    return Container(
      width: double.infinity,
      padding: const EdgeInsets.all(24),
      decoration: BoxDecoration(
        border: Border.all(color: const Color(0xFF33FF33), width: 2),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            '$_calcYear, $_calcMonth, $_calcDay',
            style: const TextStyle(
              fontFamily: 'monospace',
              fontSize: 18,
              color: Color(0xFF33FF33),
              fontWeight: FontWeight.bold,
            ),
          ),
          const SizedBox(height: 12),
          Text(
            '......IS A $_calculatedDay',
            style: const TextStyle(
              fontFamily: 'monospace',
              fontSize: 20,
              color: Color(0xFF33FF33),
              fontWeight: FontWeight.bold,
              letterSpacing: 1.5,
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildModernResult() {
    return Container(
      width: double.infinity,
      padding: const EdgeInsets.all(24),
      decoration: BoxDecoration(
        borderRadius: BorderRadius.circular(20),
        gradient: const LinearGradient(
          colors: [Color(0xFF8B5CF6), Color(0xFF4FACFE)],
          begin: Alignment.topLeft,
          end: Alignment.bottomRight,
        ),
        boxShadow: [
          BoxShadow(
            color: const Color(0xFF8B5CF6).withValues(alpha: 0.3),
            blurRadius: 16,
            offset: const Offset(0, 8),
          ),
        ],
      ),
      child: Column(
        children: [
          const Text(
            'RESULT',
            style: TextStyle(
              color: Colors.white70,
              fontSize: 12,
              fontWeight: FontWeight.bold,
              letterSpacing: 2.0,
            ),
          ),
          const SizedBox(height: 12),
          Text(
            _calculatedDay!,
            style: const TextStyle(
              color: Colors.white,
              fontSize: 36,
              fontWeight: FontWeight.w900,
              fontFamily: 'Outfit',
              letterSpacing: 1.0,
            ),
          ),
          const SizedBox(height: 8),
          Text(
            'for $_calcMonth/$_calcDay/$_calcYear',
            style: const TextStyle(
              color: Colors.white70,
              fontSize: 16,
            ),
          ),
        ],
      ),
    );
  }
}
