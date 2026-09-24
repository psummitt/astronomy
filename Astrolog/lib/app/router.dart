import 'package:go_router/go_router.dart';
import 'app_shell.dart';
import '../features/auth/presentation/login_page.dart';
import '../features/logbook/domain/observation_log.dart';
import '../features/logbook/presentation/log_detail_page.dart';
import '../features/logbook/presentation/log_form_page.dart';
import '../features/logbook/presentation/log_list_page.dart';
import '../features/planner/presentation/add_plan_page.dart';
import '../features/planner/presentation/planner_list_page.dart';
import '../features/settings/presentation/about_page.dart';
import '../features/settings/presentation/instructions_page.dart';
import '../features/settings/presentation/settings_page.dart';

final router = GoRouter(
  initialLocation: '/planner',
  routes: [
    ShellRoute(
      builder: (context, state, child) => AppShell(child: child),
      routes: [
        GoRoute(
          path: '/',
          redirect: (context, state) => '/planner',
        ),
        GoRoute(
          path: '/planner',
          builder: (context, state) => const PlannerListPage(),
          routes: [
            GoRoute(
              path: 'add',
              builder: (context, state) => const AddPlanPage(),
            ),
          ],
        ),
        GoRoute(
          path: '/logbook',
          builder: (context, state) => const LogListPage(),
          routes: [
            GoRoute(
              path: 'add',
              builder: (context, state) => const LogFormPage(),
            ),
            GoRoute(
              path: 'detail/:id',
              builder: (context, state) {
                final log = state.extra as ObservationLog;
                return LogDetailPage(log: log);
              },
            ),
            GoRoute(
              path: 'edit/:id',
              builder: (context, state) {
                final log = state.extra as ObservationLog?;
                return LogFormPage(log: log);
              },
            ),
          ],
        ),
        GoRoute(
          path: '/login',
          builder: (context, state) => const LoginPage(),
        ),
        GoRoute(
          path: '/settings',
          builder: (context, state) => const SettingsPage(),
        ),
        GoRoute(
          path: '/instructions',
          builder: (context, state) => const InstructionsPage(),
        ),
        GoRoute(
          path: '/about',
          builder: (context, state) => const AboutPage(),
        ),
      ],
    ),
  ],
);
