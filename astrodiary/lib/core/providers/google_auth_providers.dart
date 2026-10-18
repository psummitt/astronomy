import 'package:firebase_auth/firebase_auth.dart';
import 'package:flutter/foundation.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:google_sign_in/google_sign_in.dart';
import 'firebase_providers.dart';

/// OAuth 2.0 **web** client ID for the Firebase project backing AstroDiary.
///
/// Android requires this to finish the Google -> Firebase credential exchange;
/// the web and Windows builds ignore it. Find it in the Firebase console under
/// Project settings -> Your apps -> Web app -> "SDK setup and configuration" ->
/// OAuth 2.0 client ID, or in `android/app/google-services.json` under
/// `client[].oauth_client[].client_id` for the entry with no
/// `android_client_info`.
///
/// This is `null` because no web OAuth client exists for this project yet.
/// Until it is set, Android Google sign-in fails with a client configuration
/// error while web sign-in still works. See `docs/GOOGLE_SIGN_IN.md`.
const String? kGoogleServerClientId = null;

/// Whether this platform has a `google_sign_in` implementation.
///
/// The plugin ships web, Android, iOS, macOS and Windows support but has no
/// Linux implementation, where calling it throws [UnimplementedError].
bool get isGoogleSignInSupported {
  if (kIsWeb) return true;
  return defaultTargetPlatform != TargetPlatform.linux;
}

/// The narrow slice of `google_sign_in` this app depends on.
///
/// Exists so the sign-in flow can be exercised without the plugin, whose
/// [GoogleSignIn] singleton cannot be substituted directly and whose bundled
/// `testing.dart` fake still targets the removed v6 API.
abstract class GoogleAuthClient {
  /// Prepares the underlying client. Safe to call more than once.
  Future<void> initialize();

  /// Whether interactive [authenticate] is usable on this platform.
  bool get supportsAuthenticate;

  /// Runs the interactive flow. Returns the ID token, or null if Google
  /// returned none. Throws [GoogleSignInException] on cancellation or failure.
  Future<String?> authenticate();

  /// Restores a session with minimal UI. Returns null when there is none.
  Future<String?> authenticateSilently();

  Future<void> signOut();
}

class _GoogleSignInClient implements GoogleAuthClient {
  /// [GoogleSignIn.initialize] must complete before any other call and must be
  /// invoked exactly once. Static because [GoogleSignIn.instance] is a
  /// process-wide singleton that outlives any provider instance.
  static Future<void>? _initFuture;

  @override
  Future<void> initialize() {
    return _initFuture ??= GoogleSignIn.instance.initialize(
      serverClientId: kGoogleServerClientId,
    );
  }

  @override
  bool get supportsAuthenticate => GoogleSignIn.instance.supportsAuthenticate();

  @override
  Future<String?> authenticate() async {
    final GoogleSignInAccount account = await GoogleSignIn.instance.authenticate();
    return account.authentication.idToken;
  }

  @override
  Future<String?> authenticateSilently() async {
    final GoogleSignInAccount? account =
        await GoogleSignIn.instance.attemptLightweightAuthentication();
    return account?.authentication.idToken;
  }

  @override
  Future<void> signOut() => GoogleSignIn.instance.signOut();
}

/// Exchanges a Google ID token for a Firebase session.
typedef GoogleTokenVerifier = Future<void> Function(String idToken);

/// Signs a user in with Google and links the result to a Firebase session.
///
/// The Google ID token is handed to Firebase so the user ends up with a normal
/// [User]; Firestore sync and the logbook keep working unchanged.
class GoogleAuthService {
  GoogleAuthService(
    this._verifyToken,
    this._signOutFirebase, {
    GoogleAuthClient? client,
  }) : _client = client ?? _GoogleSignInClient();

  final GoogleTokenVerifier _verifyToken;
  final Future<void> Function() _signOutFirebase;
  final GoogleAuthClient _client;

  bool get isSupported => isGoogleSignInSupported;

  /// Runs the interactive Google sign-in flow and signs into Firebase.
  ///
  /// Throws [UnsupportedError] when the platform has no Google integration,
  /// [GoogleSignInException] when the user cancels or Google rejects the
  /// request, and [StateError] when Google returns no ID token.
  Future<void> signIn() async {
    if (!isSupported) {
      throw UnsupportedError(
        'Google sign-in is not available on ${defaultTargetPlatform.name}.',
      );
    }

    await _client.initialize();

    if (!_client.supportsAuthenticate) {
      throw UnsupportedError(
        'This platform does not support programmatic Google sign-in; use the '
        'sign-in UI provided by the platform.',
      );
    }

    final String? idToken = await _client.authenticate();
    if (idToken == null) {
      throw StateError('Google did not return an ID token.');
    }

    await _verifyToken(idToken);
  }

  /// Restores an existing Google session with as little UI as possible.
  ///
  /// Returns false when there is no stored session, which is the normal case on
  /// a first run. Returns true once a Firebase session has been established.
  Future<bool> signInSilently() async {
    if (!isSupported) return false;

    await _client.initialize();

    final String? idToken = await _client.authenticateSilently();
    if (idToken == null) return false;

    await _verifyToken(idToken);
    return true;
  }

  /// Signs out of Firebase and, best effort, out of Google.
  ///
  /// Google sign-out runs first so a failure there cannot leave a live Firebase
  /// session behind. Never throws.
  Future<void> signOut() async {
    try {
      await _client.signOut();
    } catch (_) {
      // No Google session on this platform, or the plugin is unavailable.
    }

    try {
      await _signOutFirebase();
    } catch (_) {
      // Firebase was never initialized; nothing to sign out of.
    }
  }
}

final googleAuthServiceProvider = Provider<GoogleAuthService>((ref) {
  // Both Firebase calls are wrapped in closures so that constructing this
  // provider never throws when Firebase failed to initialize.
  return GoogleAuthService(
    (String idToken) => ref
        .read(firebaseAuthProvider)
        .signInWithCredential(GoogleAuthProvider.credential(idToken: idToken)),
    () => ref.read(firebaseAuthProvider).signOut(),
  );
});

/// Signs the user out of both Firebase and Google.
final signOutAction = Provider<Future<void> Function()>((ref) {
  return () => ref.read(googleAuthServiceProvider).signOut();
});