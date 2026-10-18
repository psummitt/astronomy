import 'package:astrodiary/features/auth/presentation/login_page.dart';
import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:flutter_test/flutter_test.dart';

// The Google button is platform-gated: `google_sign_in` has no Linux
// implementation, so the control is hidden there rather than shown and broken.
void main() {
  Future<void> pumpLogin(WidgetTester tester) async {
    tester.view.physicalSize = const Size(1000, 1400);
    tester.view.devicePixelRatio = 1.0;
    addTearDown(() {
      tester.view.resetPhysicalSize();
      tester.view.resetDevicePixelRatio();
    });

    await tester.pumpWidget(
      const ProviderScope(child: MaterialApp(home: LoginPage())),
    );
    await tester.pumpAndSettle();
  }

  // VM only: kIsWeb is a compile-time true on the web, so the button is always
  // shown there and the Linux branch cannot be observed.
  // (`testWidgets` has no `testOn`, hence `skip`.)
  testWidgets('hides the Google button on Linux', (tester) async {
    debugDefaultTargetPlatformOverride = TargetPlatform.linux;
    try {
      await pumpLogin(tester);

      expect(find.text('Continue with Google'), findsNothing);
      expect(find.text('or'), findsNothing);
      // Other sign-in paths are unaffected.
      expect(find.text('Sign In / Register'), findsOneWidget);
      expect(find.text('Continue as Guest'), findsOneWidget);
    } finally {
      // Must be cleared before flutter_test's foundation-var invariant check,
      // which runs before any tearDown callback.
      debugDefaultTargetPlatformOverride = null;
    }
  }, skip: kIsWeb);

  testWidgets('shows Continue with Google on a supported platform',
      (tester) async {
    debugDefaultTargetPlatformOverride = TargetPlatform.android;
    try {
      await pumpLogin(tester);

      expect(find.text('Continue with Google'), findsOneWidget);
      expect(find.text('or'), findsOneWidget);
      // Email/password and guest options remain available.
      expect(find.text('Sign In / Register'), findsOneWidget);
      expect(find.text('Continue as Guest'), findsOneWidget);
    } finally {
      debugDefaultTargetPlatformOverride = null;
    }
  });
}