import 'package:flutter/material.dart';
import 'modern_calculator.dart';

void main() {
  runApp(const EasterApp());
}

class EasterApp extends StatelessWidget {
  const EasterApp({super.key});

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Easter Computus 1982',
      debugShowCheckedModeBanner: false,
      theme: ThemeData(
        brightness: Brightness.dark,
        primarySwatch: Colors.blue,
        fontFamily: 'Segoe UI',
        useMaterial3: true,
      ),
      home: const MainNavigationShell(),
    );
  }
}

class MainNavigationShell extends StatelessWidget {
  const MainNavigationShell({super.key});

  @override
  State<MainNavigationShell> createState() => _MainNavigationShellState();
}

class _MainNavigationShellState extends State<MainNavigationShell> {
  int _activeTab = 0;

  final List<Widget> _views = [
    const ModernCalculator(),
  ];

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: const Color(0xFF0F172A), // Slate 900
      appBar: AppBar(
        backgroundColor: const Color(0xFF1E293B), // Slate 800
        title: FittedBox(
          fit: BoxFit.scaleDown,
          child: Row(
            mainAxisSize: MainAxisSize.min,
            children: [
              const Icon(Icons.history_edu, color: Colors.blueAccent),
              const SizedBox(width: 12),
              const Text(
                'Easter Computus 1982',
                style: TextStyle(
                  fontWeight: FontWeight.bold,
                  fontSize: 20,
                  color: Colors.white,
                ),
              ),
              const SizedBox(width: 8),
              Container(
                padding: const EdgeInsets.symmetric(horizontal: 8, vertical: 2),
                decoration: BoxDecoration(
                  color: Colors.blueAccent.withValues(alpha: 0.2),
                  borderRadius: BorderRadius.circular(12),
                  border: Border.all(color: Colors.blueAccent.withValues(alpha: 0.4)),
                ),
                child: const Text(
                  'v1.0.0',
                  style: TextStyle(
                    fontSize: 10,
                    color: Colors.blueAccent,
                    fontWeight: FontWeight.bold,
                  ),
                ),
              )
            ],
          ),
        ),
        actions: MediaQuery.of(context).size.width > 600
            ? [
                // Desktop horizontal layout switcher
                Padding(
                  padding: const EdgeInsets.symmetric(horizontal: 16.0),
                  child: Row(
                    children: [
                      _buildHeaderNavBtn(
                        icon: Icons.dashboard_outlined,
                        label: 'Analysis Dashboard',
                        index: 0,
                      ),
                      const SizedBox(width: 8),
                      _buildHeaderNavBtn(
                        icon: Icons.terminal,
                        label: 'Retro BASIC Terminal',
                        index: 1,
                      ),
                    ],
                  ),
                )
              ]
            : null,
      ),
      body: IndexedStack(
        index: _activeTab,
        children: _views,
      ),
      bottomNavigationBar: MediaQuery.of(context).size.width <= 600
          ? BottomNavigationBar(
              currentIndex: _activeTab,
              backgroundColor: const Color(0xFF1E293B),
              selectedItemColor: Colors.blue,
              unselectedItemColor: Colors.grey[400],
              selectedLabelStyle: const TextStyle(fontWeight: FontWeight.bold),
              onTap: (index) {
                setState(() {
                  _activeTab = index;
                });
              },
              items: const [
                BottomNavigationBarItem(
                  icon: Icon(Icons.dashboard_outlined),
                  label: 'Dashboard',
                ),
                BottomNavigationBarItem(
                  icon: Icon(Icons.terminal),
                  label: 'BASIC Terminal',
                ),
              ],
            )
          : null, // Hide bottom navigation on larger viewports since we have header actions
    );
  }

  Widget _buildHeaderNavBtn({
    required IconData icon,
    required String label,
    required int index,
  }) {
    final bool isSelected = _activeTab == index;
    return TextButton.icon(
      style: TextButton.styleFrom(
        foregroundColor: isSelected ? Colors.white : Colors.grey[300],
        backgroundColor: isSelected ? Colors.blue.withValues(alpha: 0.2) : Colors.transparent,
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(8),
          side: BorderSide(
            color: isSelected ? Colors.blue : Colors.transparent,
          ),
        ),
        padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 8),
      ),
      icon: Icon(icon, size: 18, color: isSelected ? Colors.blue[300] : Colors.grey[400]),
      label: Text(
        label,
        style: const TextStyle(fontWeight: FontWeight.bold, fontSize: 13),
      ),
      onPressed: () {
        setState(() {
          _activeTab = index;
        });
      },
    );
  }
}
