package in.juspay.mobility.app;
import static io.grpc.Metadata.ASCII_STRING_MARSHALLER;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.IBinder;
import android.util.Log;

import androidx.annotation.Nullable;


import org.json.JSONException;
import org.json.JSONObject;

import java.util.Objects;
import java.util.concurrent.TimeUnit;

import io.grpc.CallOptions;
import io.grpc.Channel;
import io.grpc.ClientCall;
import io.grpc.ClientInterceptor;
import io.grpc.ForwardingClientCall;
import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.Metadata;
import io.grpc.MethodDescriptor;
import io.grpc.Status;
import io.grpc.StatusRuntimeException;
import io.grpc.stub.StreamObserver;

interface NotificationListener {
    void onError(Throwable t);
    void onMessage(NotificationPayload notification);
}

public class GRPCNotificationService extends Service implements NotificationListener {
    private static NotificationGrpc.NotificationStub asyncStub;
    private ManagedChannel channel;
    private Context context;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate() {
        super.onCreate();
        this.context = getApplicationContext();
        initialize();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Log.i("GRPC", "Destroying GRPC service");
        closeChannel();
        stopSelf();
    }

    /* Initialize channel and connection for bi-directional notification streaming */
    private void initialize() {
        SharedPreferences sharedPref = context.getSharedPreferences(context.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
        Log.i("GRPC", "Registration token - " + token);
        if(token.equals("null")){
            onDestroy();
            return;
        }
        channel = ManagedChannelBuilder.forAddress("beta.beckn.uat.juspay.net", 50051)
                .intercept(new GRPCNotificationHeaderInterceptor(token))
                .keepAliveTime(20, TimeUnit.SECONDS)
                .keepAliveTimeout(5, TimeUnit.SECONDS)
                .keepAliveWithoutCalls(true)
                .maxRetryAttempts(10)
                .enableRetry()
                .build();
        asyncStub = NotificationGrpc.newStub(channel);
        startGRPCNotificationService();
    }


    // This method starts the GRPC Notification Service
    private void startGRPCNotificationService() {
        GRPCNotificationResponseObserver notificationResponseObserver = new GRPCNotificationResponseObserver(this);
        StreamObserver<NotificationAck> notificationRequestObserver = asyncStub.streamPayload(notificationResponseObserver);
        notificationResponseObserver.startGRPCNotification(notificationRequestObserver);
    }

    // Method to close the gRPC channel
    private void closeChannel() {
        if (channel != null && !channel.isShutdown()) {
            channel.shutdownNow();
        }
    }

    // Override the finalize() method to ensure cleanup when the object is garbage collected
    @Override
    protected void finalize() throws Throwable {
        try {
            closeChannel();
        } finally {
            super.finalize();
        }
    }

    @Override
    public void onError(Throwable t) {
        Log.e("GRPC", "[Retrying]");
        if(t instanceof StatusRuntimeException){
            StatusRuntimeException statusRuntimeException = (StatusRuntimeException) t;
            if(statusRuntimeException.getStatus().getCode() == Status.Code.UNAVAILABLE &&
                    (statusRuntimeException.getStatus().getDescription() != null ? statusRuntimeException.getStatus().getDescription().contains("Unable to resolve host") : false)){

                stopSelf();
                return;
            }
        }

        Log.i("GRPC", "Reached here in onError");
        closeChannel();
        initialize();
    }

    @Override
    public void onMessage(NotificationPayload notification) {
        Log.e("GRPC", "[Processing]");

        try {
            JSONObject entity_payload = new JSONObject(notification.getEntity().getData());
            JSONObject payload = new JSONObject();
            String notificationType = notification.getCategory();

            String title = notification.getTitle();
            String body = notification.getBody();

            payload.put("notification_id", notification.getId());
            payload.put("notification_type", notification.getCategory());
            payload.put("entity_ids", notification.getEntity().getId());
            payload.put("entity_type", notification.getEntity().getType());
            payload.put("show_notification", Objects.equals(notification.getShow(), "SHOW") ? "true" : "false");


            if(notificationType.equals("NEW_RIDE_AVAILABLE")) {
                JSONObject notificationData = new JSONObject();
                notificationData.put("title", title)
                        .put("msg", body);
                NotificationUtils.triggerUICallbacks(notificationType, notificationData.toString());

                SharedPreferences sharedPref = getApplicationContext().getSharedPreferences(this.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
                RideRequestUtils.addRideReceivedEvent(entity_payload, null, null, "ride_request_fcm_received", this);
                if (sharedPref.getString("DISABLE_WIDGET", "null").equals("true") && sharedPref.getString("REMOVE_CONDITION", "false").equals("false")) {
                    if (sharedPref.getString("ACTIVITY_STATUS", "null").equals("onDestroy"))
                        NotificationUtils.showRR(this, entity_payload, payload, "GRPC");
                    else {
                        RideRequestUtils.addRideReceivedEvent(entity_payload, null, null, "ride_request_ignored", this);
                        NotificationUtils.firebaseLogEventWithParams(this, "ride_ignored", "payload", entity_payload.toString());
                    }
                } else NotificationUtils.showRR(this, entity_payload, payload, "GRPC");
            }

        } catch (JSONException e) {
            e.printStackTrace();
        }
    }
}

/***
 * GRPCNotificationResponseObserver
 * This class is responsible to observe the stream of the bidirectional communication happening between client application and GRPC server
 * Implements StreamObserver class as the duplex communication will be in stream
 * ***/
class GRPCNotificationResponseObserver implements StreamObserver<NotificationPayload> {
    private StreamObserver<NotificationAck> notificationRequestObserver;
    private final NotificationListener notificationListener;

    public GRPCNotificationResponseObserver(NotificationListener notificationListener) {
        this.notificationListener = notificationListener;
    }

    /***
     * This method is responsible for initiating the connection to the server ( initially the acknowledgement will be sent for a random notification id )'''
     * ***/
    public void startGRPCNotification( StreamObserver<NotificationAck> notificationRequestObserver){
        this.notificationRequestObserver = notificationRequestObserver;
        Log.i("GRPC", "[Started]");
        this.notificationRequestObserver.onNext(NotificationAck.newBuilder().setId("").build());
    }

    @Override
    public void onNext(NotificationPayload notification) {
        Log.i("GRPC", "[Message] : " + notification.toString());
        this.notificationRequestObserver.onNext(NotificationAck.newBuilder().setId(notification.getId()).build());
        notificationListener.onMessage(notification);
    }

    @Override
    public void onError(Throwable t) {
        Log.e("GRPC", "[Error] : " + t.toString());
        notificationListener.onError(t);
    }

    @Override
    public void onCompleted() {
        Log.e("GRPC", "[Completed]");
        notificationListener.onError(null);
    }
}

/***
 * GRPCNotificationHeaderInterceptor
 * This class is responsible for modifying the message ( here adding header in the message ) before sending the message to the server in duplex communication
 * ***/
class GRPCNotificationHeaderInterceptor implements ClientInterceptor {
    final private String token;
    GRPCNotificationHeaderInterceptor(String token) {
        this.token = token;
    }

    @Override
    public <ReqT, RespT> ClientCall<ReqT, RespT> interceptCall(MethodDescriptor<ReqT, RespT> method, CallOptions callOptions, Channel next) {
        return new ForwardingClientCall.SimpleForwardingClientCall<ReqT, RespT>(next.newCall(method, callOptions)) {

            @Override
            public void start(Listener<RespT> responseListener, Metadata headers) {
                headers.put(Metadata.Key.of("token", ASCII_STRING_MARSHALLER), token);
                super.start(responseListener, headers);
            }
        };
    }
}
