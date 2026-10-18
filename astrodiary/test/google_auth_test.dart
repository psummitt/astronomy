import 'package:astrodiary/core/providers/google_auth_providers.dart';
import 'package:flutter/foundation.dart';
import 'package:flutter_test/flutter_test.dart';

/// Records what the service asked Google for and returns canned results.
class FakeGoogleAuthClient implements GoogleAuthClient {
  FakeGoogleAuthClient({
    this.idToken = 'test-id-token',
    this.silentIdToken,
    this.canAuthenticate = true,
    this.initializeCalls = 0,
    this.signOutError,
    this.authenticateError,
  });

  String? idToken;
  String? silentIdToken;
  bool canAuthenticate;
  int initializeCalls;

  Object? signOutError;
  Object? authenticateError;

  bool signedOut = false;
  bool silentAttempted = false;

  @override
  Future<void> initialize() async => initializeCalls++;

  @override
  bool get supportsAuthenticate => canAuthenticate;

  @override
  Future<String?> authenticate() async {
    if (authenticateError != null) throw authenticateError!;
    return idToken;
  }

  @override
  Future<String?> authenticateSilently() async {
    silentAttempted = true;
    return silentIdToken;
  }

  @override
  Future<void> signOut() async {
    if (signOutError != null) throw signOutError!;
    signedOut = true;
  }
}

GoogleAuthService buildService({
  required FakeGoogleAuthClient client,
  List<String>? verified,
  void Function()? onSignOutFirebase,
}) {
  return GoogleAuthService(
    (String idToken) async => verified?.add(idToken),
    () async => onSignOutFirebase?.call(),
    client: client,
  );
}

void main() {
  tearDown(() {
    debugDefaultTargetPlatformOverride = null;
  });

  group('platform support', () {
    // `kIsWeb` is a compile-time `true` on the web, so the Linux branch of
    // `isGoogleSignInSupported` is unreachable there. These run on the VM only.
    test('Linux has no google_sign_in implementation', () {
      debugDefaultTargetPlatformOverride = TargetPlatform.linux;
      expect(isGoogleSignInSupported, isFalse);
    }, testOn: 'vm');

    test('Android, iOS, Windows and macOS are supported', () {
      for (final platform in [
        TargetPlatform.android,
        TargetPlatform.iOS,
        TargetPlatform.windows,
        TargetPlatform.macOS,
      ]) {
        debugDefaultTargetPlatformOverride = platform;
        expect(isGoogleSignInSupported, isTrue, reason: platform.name);
      }
    });

    test('signIn throws UnsupportedError and never touches Google on Linux',
        () async {
      debugDefaultTargetPlatformOverride = TargetPlatform.linux;
      final client = FakeGoogleAuthClient();
      final service = buildService(client: client, verified: []);

      await expectLater(service.signIn(), throwsUnsupportedError);
      // Guard against a half-initialised client being used later.
      expect(client.initializeCalls, 0);
    }, testOn: 'vm');

    test('signInSilently is a no-op on Linux', () async {
      debugDefaultTargetPlatformOverride = TargetPlatform.linux;
      final client = FakeGoogleAuthClient();
      final service = buildService(client: client, verified: []);

      expect(await service.signInSilently(), isFalse);
      expect(client.silentAttempted, isFalse);
    }, testOn: 'vm');
  });

  group('signIn', () {
    setUp(() => debugDefaultTargetPlatformOverride = TargetPlatform.android);

    test('passes the Google ID token through to Firebase', () async {
      final verified = <String>[];
      final client = FakeGoogleAuthClient(idToken: 'abc123');
      final service = buildService(client: client, verified: verified);

      await service.signIn();

      expect(verified, ['abc123']);
    });

    test('initializes before authenticating; the client dedupes repeats',
        () async {
      final client = FakeGoogleAuthClient();
      final service = buildService(client: client, verified: []);

      await service.signIn();
      await service.signIn();

      // The service always initializes first; GoogleSignIn.initialize must run
      // exactly once per process, which _GoogleSignInClient guarantees with its
      // static _initFuture rather than the service.
      expect(client.initializeCalls, 2);
    });

    test('throws when the platform cannot show sign-in UI', () async {
      final client = FakeGoogleAuthClient(canAuthenticate: false);
      final service = buildService(client: client, verified: []);

      await expectLater(service.signIn(), throwsUnsupportedError);
    });

    test('throws when Google returns no ID token', () async {
      final client = FakeGoogleAuthClient(idToken: null);
      final service = buildService(client: client, verified: []);

      await expectLater(service.signIn(), throwsStateError);
    });

    test('propagates a cancellation from Google', () async {
      final client = FakeGoogleAuthClient(
        authenticateError: StateError('canceled'),
      );
      final service = buildService(client: client, verified: []);

      await expectLater(service.signIn(), throwsStateError);
    });
  });

  group('signInSilently', () {
    setUp(() => debugDefaultTargetPlatformOverride = TargetPlatform.android);

    test('returns false when there is no stored session', () async {
      final client = FakeGoogleAuthClient(silentIdToken: null);
      final verified = <String>[];
      final service = buildService(client: client, verified: verified);

      expect(await service.signInSilently(), isFalse);
      expect(verified, isEmpty);
    });

    test('verifies the token and returns true when a session exists', () async {
      final verified = <String>[];
      final client = FakeGoogleAuthClient(silentIdToken: 'silent-token');
      final service = buildService(client: client, verified: verified);

      expect(await service.signInSilently(), isTrue);
      expect(verified, ['silent-token']);
    });
  });

  group('signOut', () {
    setUp(() => debugDefaultTargetPlatformOverride = TargetPlatform.android);

    test('signs out of Google and Firebase', () async {
      var firebaseSignedOut = false;
      final client = FakeGoogleAuthClient();
      final service = buildService(
        client: client,
        onSignOutFirebase: () => firebaseSignedOut = true,
      );

      await service.signOut();

      expect(client.signedOut, isTrue);
      expect(firebaseSignedOut, isTrue);
    });

    test('still signs out of Firebase when Google sign-out throws', () async {
      var firebaseSignedOut = false;
      final client = FakeGoogleAuthClient(signOutError: StateError('boom'));
      final service = buildService(
        client: client,
        onSignOutFirebase: () => firebaseSignedOut = true,
      );

      await service.signOut();

      expect(firebaseSignedOut, isTrue);
    });

    test('never throws when both sides fail (Firebase not initialized)', () async {
      final client = FakeGoogleAuthClient(signOutError: StateError('boom'));
      final service = GoogleAuthService(
        (_) async {},
        () async => throw StateError('no Firebase app'),
        client: client,
      );

      await expectLater(service.signOut(), completes);
    });
  });
}