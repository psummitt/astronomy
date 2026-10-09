import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import '../core/providers/auth_providers.dart';

class AppShell extends ConsumerWidget {
  const AppShell({
    super.key,
    required this.child,
  });

  final Widget child;

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final size = MediaQuery.of(context).size;
    final isWide = size.width > 700;
    final user = ref.watch(currentUserProvider);

    final selectedIndex = _getSelectedIndex(context);

    return Scaffold(
      body: Row(
        children: [
          if (isWide)
            NavigationRail(
              extended: size.width > 950,
              destinations: [
                const NavigationRailDestination(
                  icon: Icon(Icons.calendar_month_outlined),
                  selectedIcon: Icon(Icons.calendar_month),
                  label: Text('Calendar'),
                ),
                const NavigationRailDestination(
                  icon: Icon(Icons.auto_awesome_outlined),
                  selectedIcon: Icon(Icons.auto_awesome),
                  label: Text('Planner'),
                ),
                const NavigationRailDestination(
                  icon: Icon(Icons.menu_book_outlined),
                  selectedIcon: Icon(Icons.menu_book),
                  label: Text('Logbook'),
                ),
                const NavigationRailDestination(
                  icon: Icon(Icons.help_outline),
                  selectedIcon: Icon(Icons.help),
                  label: Text('Guide'),
                ),
                NavigationRailDestination(
                  icon: const Icon(Icons.settings_outlined),
                  selectedIcon: const Icon(Icons.settings),
                  label: Text(user == null ? 'Settings' : 'Profile'),
                ),
              ],
              selectedIndex: selectedIndex,
              onDestinationSelected: (index) => _onItemTapped(index, context),
            ),
          Expanded(child: child),
        ],
      ),
      bottomNavigationBar: isWide
          ? null
          : NavigationBar(
              destinations: [
                const NavigationDestination(
                  icon: Icon(Icons.calendar_month_outlined),
                  selectedIcon: Icon(Icons.calendar_month),
                  label: 'Calendar',
                ),
                const NavigationDestination(
                  icon: Icon(Icons.auto_awesome_outlined),
                  selectedIcon: Icon(Icons.auto_awesome),
                  label: 'Planner',
                ),
                const NavigationDestination(
                  icon: Icon(Icons.menu_book_outlined),
                  selectedIcon: Icon(Icons.menu_book),
                  label: 'Logbook',
                ),
                const NavigationDestination(
                  icon: Icon(Icons.help_outline),
                  selectedIcon: Icon(Icons.help),
                  label: 'Guide',
                ),
                NavigationDestination(
                  icon: const Icon(Icons.settings_outlined),
                  selectedIcon: const Icon(Icons.settings),
                  label: user == null ? 'Settings' : 'Profile',
                ),
              ],
              selectedIndex: selectedIndex,
              onDestinationSelected: (index) => _onItemTapped(index, context),
            ),
    );
  }

  int _getSelectedIndex(BuildContext context) {
    final location = GoRouterState.of(context).uri.path;
    if (location.startsWith('/calendar')) return 0;
    if (location.startsWith('/planner')) return 1;
    if (location.startsWith('/logbook')) return 2;
    if (location.startsWith('/instructions')) return 3;
    if (location.startsWith('/settings') || location.startsWith('/login')) return 4;
    return 0;
  }

  void _onItemTapped(int index, BuildContext context) {
    switch (index) {
      case 0:
        context.go('/calendar');
        break;
      case 1:
        context.go('/planner');
        break;
      case 2:
        context.go('/logbook');
        break;
      case 3:
        context.go('/instructions');
        break;
      case 4:
        context.go('/settings');
        break;
    }
  }
}
