import 'package:flutter/material.dart';
import 'easter_algorithms.dart';

class ModernCalculator extends StatefulWidget {
  const ModernCalculator({super.key});

  @override
  State<ModernCalculator> createState() => _ModernCalculatorState();
}

class _ModernCalculatorState extends State<ModernCalculator> {
  int _selectedYear = 2026;
  final TextEditingController _yearController = TextEditingController(text: '2026');
  bool _showMismatchesOnly = false;

  @override
  void initState() {
    super.initState();
  }

  @override
  void dispose() {
    _yearController.dispose();
    super.dispose();
  }

  void _selectYear(int year) {
    setState(() {
      _selectedYear = year;
      _yearController.text = year.toString();
    });
  }

  // Pre-calculate mismatches count
  int get _totalMismatches {
    int count = 0;
    for (int y = 1900; y <= 2099; y++) {
      if (standardEaster(y) != burgessEaster(y)) {
        count++;
      }
    }
    return count;
  }

  @override
  Widget build(BuildContext context) {
    final EasterDate standardResult = standardEaster(_selectedYear);
    final EasterDate burgessResult = burgessEaster(_selectedYear);
    final bool isMatch = standardResult == burgessResult;

    // Calculate absolute difference in days if there is a mismatch
    int dayDifference = 0;
    if (!isMatch) {
      final DateTime dtStd = standardResult.toDateTime();
      final DateTime dtBsc = burgessResult.toDateTime();
      dayDifference = dtStd.difference(dtBsc).inDays.abs();
    }

    return LayoutBuilder(
      builder: (context, constraints) {
        final bool isDesktop = constraints.maxWidth > 800;

        return Scaffold(
          backgroundColor: const Color(0xFF0F172A), // Tailwind slate-900 background
          body: SingleChildScrollView(
            child: Padding(
              padding: const EdgeInsets.all(24.0),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  // Title / Header
                  _buildHeader(),
                  const SizedBox(height: 24),

                  // Main Content: Inputs & Calculations
                  if (isDesktop)
                    Row(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Expanded(
                          flex: 4,
                          child: Column(
                            children: [
                              _buildYearSelectorCard(),
                              const SizedBox(height: 24),
                              _buildResultsRow(standardResult, burgessResult, isMatch, dayDifference),
                              const SizedBox(height: 24),
                              _buildEducationalCard(),
                            ],
                          ),
                        ),
                        const SizedBox(width: 24),
                        Expanded(
                          flex: 3,
                          child: _buildYearDirectory(),
                        ),
                      ],
                    )
                  else
                    Column(
                      children: [
                        _buildYearSelectorCard(),
                        const SizedBox(height: 24),
                        _buildResultsRow(standardResult, burgessResult, isMatch, dayDifference),
                        const SizedBox(height: 24),
                        _buildEducationalCard(),
                        const SizedBox(height: 24),
                        _buildYearDirectory(),
                      ],
                    ),
                ],
              ),
            ),
          ),
        );
      },
    );
  }

  Widget _buildHeader() {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.start,
      children: [
        Row(
          children: [
            Container(
              padding: const EdgeInsets.all(8),
              decoration: BoxDecoration(
                color: Colors.blue.withValues(alpha: 0.15),
                borderRadius: BorderRadius.circular(12),
              ),
              child: const Icon(Icons.wb_sunny_outlined, color: Colors.blue, size: 28),
            ),
            const SizedBox(width: 12),
            const Text(
              'Easter Computus Analysis',
              style: TextStyle(
                color: Colors.white,
                fontSize: 28,
                fontWeight: FontWeight.bold,
                letterSpacing: -0.5,
              ),
            ),
          ],
        ),
        const SizedBox(height: 8),
        Text(
          'Comparing standard Gregorian Easter with Eric Burgess\'s 1982 Celestial BASIC algorithm.',
          style: TextStyle(
            color: const Color(0xFF94A3B8), // slate-400
            fontSize: 15,
          ),
        ),
      ],
    );
  }

  Widget _buildYearSelectorCard() {
    return Card(
      color: const Color(0xFF1E293B), // slate-800
      shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(16)),
      elevation: 4,
      child: Padding(
        padding: const EdgeInsets.all(24.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            const Text(
              'Select Calendar Year',
              style: TextStyle(color: Colors.white, fontSize: 18, fontWeight: FontWeight.bold),
            ),
            const SizedBox(height: 8),
            Text(
              'Select any year within the BASIC program\'s bounds (1900 - 2099)',
              style: TextStyle(color: const Color(0xFF94A3B8), fontSize: 13),
            ),
            const SizedBox(height: 20),
            Row(
              children: [
                Expanded(
                  child: SliderTheme(
                    data: SliderTheme.of(context).copyWith(
                      activeTrackColor: Colors.blue,
                      inactiveTrackColor: const Color(0xFF334155), // slate-700
                      thumbColor: Colors.blueAccent,
                      overlayColor: Colors.blue.withValues(alpha: 0.2),
                      valueIndicatorColor: Colors.blue,
                    ),
                    child: Slider(
                      value: _selectedYear.toDouble(),
                      min: 1900,
                      max: 2099,
                      divisions: 199,
                      label: _selectedYear.toString(),
                      onChanged: (val) {
                        _selectYear(val.round());
                      },
                    ),
                  ),
                ),
                const SizedBox(width: 16),
                SizedBox(
                  width: 80,
                  child: TextField(
                    controller: _yearController,
                    keyboardType: TextInputType.number,
                    style: const TextStyle(color: Colors.white, fontWeight: FontWeight.bold),
                    textAlign: TextAlign.center,
                    decoration: InputDecoration(
                      fillColor: const Color(0xFF0F172A),
                      filled: true,
                      contentPadding: const EdgeInsets.symmetric(vertical: 8, horizontal: 4),
                      enabledBorder: OutlineInputBorder(
                        borderSide: const BorderSide(color: Color(0xFF334155)),
                        borderRadius: BorderRadius.circular(8),
                      ),
                      focusedBorder: OutlineInputBorder(
                        borderSide: const BorderSide(color: Colors.blue),
                        borderRadius: BorderRadius.circular(8),
                      ),
                    ),
                    onSubmitted: (val) {
                      final int? year = int.tryParse(val);
                      if (year != null && year >= 1900 && year <= 2099) {
                        setState(() {
                          _selectedYear = year;
                        });
                      } else {
                        _yearController.text = _selectedYear.toString();
                      }
                    },
                  ),
                ),
              ],
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildResultsRow(
      EasterDate std, EasterDate burgess, bool isMatch, int diffDays) {
    return Column(
      children: [
        // Comparison Match/Mismatch Banner
        Container(
          width: double.infinity,
          padding: const EdgeInsets.symmetric(vertical: 16, horizontal: 20),
          decoration: BoxDecoration(
            color: isMatch
                ? const Color(0xFF10B981).withValues(alpha: 0.1) // emerald
                : const Color(0xFFF43F5E).withValues(alpha: 0.1), // rose
            borderRadius: BorderRadius.circular(12),
            border: Border.all(
              color: isMatch
                  ? const Color(0xFF10B981).withValues(alpha: 0.3)
                  : const Color(0xFFF43F5E).withValues(alpha: 0.3),
            ),
          ),
          child: Row(
            children: [
              Icon(
                isMatch ? Icons.check_circle_outline : Icons.error_outline,
                color: isMatch ? const Color(0xFF10B981) : const Color(0xFFF43F5E),
                size: 24,
              ),
              const SizedBox(width: 12),
              Expanded(
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(
                      isMatch ? 'ALGORITHMS MATCH' : 'ALGORITHMS MISMATCH',
                      style: TextStyle(
                        color: isMatch ? const Color(0xFF10B981) : const Color(0xFFF43F5E),
                        fontWeight: FontWeight.bold,
                        fontSize: 14,
                        letterSpacing: 0.5,
                      ),
                    ),
                    const SizedBox(height: 2),
                    Text(
                      isMatch
                          ? 'Both algorithms agree on this date.'
                          : 'The original 1982 algorithm yields a date that differs by $diffDays ${diffDays == 1 ? "day" : "days"}.',
                      style: TextStyle(
                        color: isMatch ? const Color(0xFFD1FAE5) : const Color(0xFFFFE4E6), // emerald-100 / rose-100
                        fontSize: 13,
                      ),
                    ),
                  ],
                ),
              ),
            ],
          ),
        ),
        const SizedBox(height: 16),

        // Double Cards
        Row(
          children: [
            Expanded(
              child: _buildCalculationCard(
                title: 'STANDARD GREGORIAN',
                subtitle: 'Meeus/Jones/Butcher',
                dateStr: '${std.monthName} ${std.day}',
                isCorrect: true,
                gradient: const LinearGradient(
                  colors: [Color(0xFF2563EB), Color(0xFF1D4ED8)], // Blue
                  begin: Alignment.topLeft,
                  end: Alignment.bottomRight,
                ),
              ),
            ),
            const SizedBox(width: 16),
            Expanded(
              child: _buildCalculationCard(
                title: 'ORIGINAL BASIC',
                subtitle: 'Eric Burgess (1982)',
                dateStr: '${burgess.monthName} ${burgess.day}',
                isCorrect: isMatch,
                gradient: LinearGradient(
                  colors: isMatch
                      ? [const Color(0xFF059669), const Color(0xFF047857)] // Green if matches
                      : [const Color(0xFFD97706), const Color(0xFFB45309)], // Amber if mismatch
                  begin: Alignment.topLeft,
                  end: Alignment.bottomRight,
                ),
              ),
            ),
          ],
        ),
      ],
    );
  }

  Widget _buildCalculationCard({
    required String title,
    required String subtitle,
    required String dateStr,
    required bool isCorrect,
    required Gradient gradient,
  }) {
    return Container(
      height: 150,
      decoration: BoxDecoration(
        gradient: gradient,
        borderRadius: BorderRadius.circular(16),
        boxShadow: [
          BoxShadow(
            color: Colors.black.withValues(alpha: 0.15),
            blurRadius: 10,
            offset: const Offset(0, 4),
          )
        ],
      ),
      child: Stack(
        children: [
          Positioned(
            right: -20,
            bottom: -20,
            child: Opacity(
              opacity: 0.15,
              child: Icon(
                isCorrect ? Icons.calendar_today : Icons.warning_amber_rounded,
                size: 130,
                color: Colors.white,
              ),
            ),
          ),
          Padding(
            padding: const EdgeInsets.all(20.0),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.start,
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(
                      title,
                      style: const TextStyle(
                        color: Colors.white,
                        fontSize: 12,
                        fontWeight: FontWeight.bold,
                        letterSpacing: 1.0,
                      ),
                    ),
                    Text(
                      subtitle,
                      style: TextStyle(
                        color: Colors.white.withValues(alpha: 0.7),
                        fontSize: 11,
                      ),
                    ),
                  ],
                ),
                Text(
                  dateStr,
                  style: const TextStyle(
                    color: Colors.white,
                    fontSize: 26,
                    fontWeight: FontWeight.bold,
                    shadows: [
                      Shadow(
                        color: Colors.black26,
                        offset: Offset(1, 2),
                        blurRadius: 3,
                      )
                    ],
                  ),
                ),
              ],
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildEducationalCard() {
    return Card(
      color: const Color(0xFF1E293B), // slate-800
      shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(16)),
      elevation: 4,
      child: Padding(
        padding: const EdgeInsets.all(24.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                const Icon(Icons.menu_book_outlined, color: Colors.blue),
                const SizedBox(width: 10),
                const Text(
                  'Analysis of the Algorithm Bug',
                  style: TextStyle(
                    color: Colors.white,
                    fontSize: 18,
                    fontWeight: FontWeight.bold,
                  ),
                ),
              ],
            ),
            const SizedBox(height: 16),
            const Text(
              'Why does the original BASIC code fail for 59 years?',
              style: TextStyle(color: Colors.white, fontWeight: FontWeight.w600, fontSize: 14),
            ),
            const SizedBox(height: 8),
            Text(
              'The algorithm implemented by Eric Burgess in Celestial BASIC was meant to be a simplified Computus calculation for the 1900-2099 window. In standard modular integer math, the logic is actually 100% correct.',
              style: TextStyle(color: const Color(0xFFCBD5E1), fontSize: 13, height: 1.5),
            ),
            const SizedBox(height: 8),
            Text(
              'However, because the code was run in Applesoft/TS-1000 floating-point BASIC, it converted divisions to floats and attempted to extract the fractional part to mimic modulo operations:',
              style: TextStyle(color: const Color(0xFFCBD5E1), fontSize: 13, height: 1.5),
            ),
            const SizedBox(height: 12),
            Container(
              width: double.infinity,
              padding: const EdgeInsets.all(12),
              decoration: BoxDecoration(
                color: const Color(0xFF0F172A),
                borderRadius: BorderRadius.circular(8),
                border: Border.all(color: const Color(0xFF334155)),
              ),
              child: const Text(
                '220 M = (11 * A + 4.00001 - B) / 29\n'
                '230 X = M - INT (M)\n'
                '250 IF X <> 1 THEN M = 29 * X',
                style: TextStyle(
                  color: Color(0xFF38BDF8),
                  fontFamily: 'Courier',
                  fontSize: 13,
                  height: 1.4,
                ),
              ),
            ),
            const SizedBox(height: 12),
            Text(
              'Due to floating point limitations, small precision errors accumulated. For instance, when X was supposed to equal 0, it became very small, causing M (which represents lunar cycle values) to be offset. This shifted the final date calculations by exactly 1 week (7 days) or 1 day for 59 different years.',
              style: TextStyle(color: const Color(0xFFCBD5E1), fontSize: 13, height: 1.5),
            ),
            const SizedBox(height: 8),
            Text(
              'To patch this, Eric Burgess hardcoded manual overrides for 1974, 1984, and 1994 (lines 430 & 440) which was a "kludge" to mask the rounding errors on specific dates he checked, leaving the remaining 56 mismatched years uncorrected in the book.',
              style: TextStyle(color: const Color(0xFFCBD5E1), fontSize: 13, height: 1.5),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildYearDirectory() {
    return Card(
      color: const Color(0xFF1E293B),
      shape: RoundedRectangleBorder(borderRadius: BorderRadius.circular(16)),
      elevation: 4,
      child: Padding(
        padding: const EdgeInsets.all(20.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              mainAxisAlignment: MainAxisAlignment.spaceBetween,
              children: [
                Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    const Text(
                      'Year Directory',
                      style: TextStyle(color: Colors.white, fontSize: 18, fontWeight: FontWeight.bold),
                    ),
                    Text(
                      '$_totalMismatches mismatches highlighted',
                      style: TextStyle(color: const Color(0xFF94A3B8), fontSize: 12),
                    ),
                  ],
                ),
                // Toggle Filter
                FilterChip(
                  label: const Text('Mismatches Only', style: TextStyle(fontSize: 12)),
                  selected: _showMismatchesOnly,
                  selectedColor: const Color(0xFFF43F5E).withValues(alpha: 0.25),
                  checkmarkColor: const Color(0xFFF43F5E),
                  labelStyle: TextStyle(
                    color: _showMismatchesOnly ? const Color(0xFFFECDD3) : const Color(0xFFCBD5E1),
                    fontWeight: FontWeight.bold,
                  ),
                  backgroundColor: const Color(0xFF0F172A),
                  onSelected: (selected) {
                    setState(() {
                      _showMismatchesOnly = selected;
                    });
                  },
                ),
              ],
            ),
            const SizedBox(height: 16),
            const Divider(color: Color(0xFF334155), height: 1),
            const SizedBox(height: 8),

            // Scrollable directory list
            SizedBox(
              height: 480,
              child: ListView.separated(
                itemCount: 200, // 1900 to 2099
                separatorBuilder: (context, index) {
                  final int year = 1900 + index;
                  final bool stdMatch = standardEaster(year) == burgessEaster(year);
                  final bool stdMatchNext = standardEaster(year + 1) == burgessEaster(year + 1);

                  if (_showMismatchesOnly && (stdMatch || stdMatchNext)) {
                    return const SizedBox.shrink();
                  }
                  return const Divider(color: Color(0xFF334155), height: 1);
                },
                itemBuilder: (context, index) {
                  final int year = 1900 + index;
                  final EasterDate std = standardEaster(year);
                  final EasterDate bsc = burgessEaster(year);
                  final bool matches = std == bsc;

                  if (_showMismatchesOnly && matches) {
                    return const SizedBox.shrink();
                  }

                  final bool isSelected = year == _selectedYear;

                  return InkWell(
                    onTap: () => _selectYear(year),
                    borderRadius: BorderRadius.circular(8),
                    child: Container(
                      padding: const EdgeInsets.symmetric(vertical: 12, horizontal: 8),
                      decoration: BoxDecoration(
                        color: isSelected
                            ? Colors.blue.withValues(alpha: 0.15)
                            : Colors.transparent,
                        borderRadius: BorderRadius.circular(8),
                      ),
                      child: Row(
                        mainAxisAlignment: MainAxisAlignment.spaceBetween,
                        children: [
                          Text(
                            year.toString(),
                            style: TextStyle(
                              color: isSelected ? Colors.blue[300] : Colors.white,
                              fontWeight: FontWeight.bold,
                              fontSize: 16,
                            ),
                          ),
                          Column(
                            crossAxisAlignment: CrossAxisAlignment.end,
                            children: [
                              Text(
                                'Std: ${std.monthName.substring(0, 3)} ${std.day}',
                                style: TextStyle(
                                  color: const Color(0xFFCBD5E1),
                                  fontSize: 13,
                                  fontWeight: FontWeight.w500,
                                ),
                              ),
                              const SizedBox(height: 2),
                              Text(
                                'Basic: ${bsc.monthName.substring(0, 3)} ${bsc.day}',
                                style: TextStyle(
                                  color: matches ? const Color(0xFF6EE7B7) : const Color(0xFFFCA5A5),
                                  fontSize: 12,
                                  fontWeight: matches ? FontWeight.normal : FontWeight.bold,
                                ),
                              ),
                            ],
                          ),
                        ],
                      ),
                    ),
                  );
                },
              ),
            ),
          ],
        ),
      ),
    );
  }
}
