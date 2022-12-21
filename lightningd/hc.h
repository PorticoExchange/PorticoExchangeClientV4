package fr.acinq.eclair.wire.internal.channel.version3

import fr.acinq.eclair.wire.protocol.CommonCodecs._
import fr.acinq.eclair.wire.protocol.LightningMessageCodecs._
import fr.acinq.eclair.wire.protocol._
import fr.acinq.hc.app.HC._
import fr.acinq.hc.app._
import scodec.codecs._
import scodec.{Attempt, Err}


object HCProtocolCodecs {
  val invokeHostedChannelCodec = {
    (bytes32 withContext "chainHash") ::
      (varsizebinarydata withContext "refundScriptPubKey") ::
      (varsizebinarydata withContext "secret")
  }.as[InvokeHostedChannel]

  val initHostedChannelCodec = {
    (uint64 withContext "maxHtlcValueInFlightMsat") ::
      (millisatoshi withContext "htlcMinimumMsat") ::
      (uint16 withContext "maxAcceptedHtlcs") ::
      (millisatoshi withContext "channelCapacityMsat") ::
      (millisatoshi withContext "initialClientBalanceMsat") ::
      (listOfN(uint16, uint16) withContext "features")
  }.as[InitHostedChannel]

  val hostedChannelBrandingCodec = {
    (rgb withContext "rgbColor") ::
      (optional(bool8, varsizebinarydata) withContext "pngIcon") ::
      (variableSizeBytes(uint16, utf8) withContext "contactInfo")
  }.as[HostedChannelBranding]

  lazy val lastCrossSignedStateCodec = {
    (bool8 withContext "isHost") ::
      (varsizebinarydata withContext "refundScriptPubKey") ::
      (lengthDelimited(initHostedChannelCodec) withContext "initHostedChannel") ::
      (uint32 withContext "blockDay") ::
      (millisatoshi withContext "localBalanceMsat") ::
      (millisatoshi withContext "remoteBalanceMsat") ::
      (uint32 withContext "localUpdates") ::
      (uint32 withContext "remoteUpdates") ::
      (listOfN(uint16, lengthDelimited(updateAddHtlcCodec)) withContext "incomingHtlcs") ::
      (listOfN(uint16, lengthDelimited(updateAddHtlcCodec)) withContext "outgoingHtlcs") ::
      (bytes64 withContext "remoteSigOfLocal") ::
      (bytes64 withContext "localSigOfRemote")
  }.as[LastCrossSignedState]

  val stateUpdateCodec = {
    (uint32 withContext "blockDay") ::
      (uint32 withContext "localUpdates") ::
      (uint32 withContext "remoteUpdates") ::
      (bytes64 withContext "localSigOfRemoteLCSS")
  }.as[StateUpdate]

  val stateOverrideCodec = {
    (uint32 withContext "blockDay") ::
      (millisatoshi withContext "localBalanceMsat") ::
      (uint32 withContext "localUpdates") ::
      (uint32 withContext "remoteUpdates") ::
      (bytes64 withContext "localSigOfRemoteLCSS")
  }.as[StateOverride]

  val announcementSignatureCodec = {
    (bytes64 withContext "nodeSignature") ::
      (bool8 withContext "wantsReply")
  }.as[AnnouncementSignature]

  val resizeChannelCodec = {
    (satoshi withContext "newCapacity") ::
      (bytes64 withContext "clientSig")
  }.as[ResizeChannel]

  val askBrandingInfoCodec = (bytes32 withContext "chainHash").as[AskBrandingInfo]

  val queryPublicHostedChannelsCodec = (bytes32 withContext "chainHash").as[QueryPublicHostedChannels]

  val replyPublicHostedChannelsEndCodec = (bytes32 withContext "chainHash").as[ReplyPublicHostedChannelsEnd]

  val queryPreimagesCodec = (listOfN(uint16, bytes32) withContext "hashes").as[QueryPreimages]

  val replyPreimagesCodec = (listOfN(uint16, bytes32) withContext "preimages").as[ReplyPreimages]

  // HC messages which don't have channel id

  def decodeHostedMessage(wrap: UnknownMessage): Attempt[HostedChannelMessage] = {
    val bitVector = wrap.data.toBitVector

    val decodeAttempt = wrap.tag match {
      case HC_STATE_UPDATE_TAG => stateUpdateCodec.decode(bitVector)
      case HC_STATE_OVERRIDE_TAG => stateOverrideCodec.decode(bitVector)
      case HC_RESIZE_CHANNEL_TAG => resizeChannelCodec.decode(bitVector)
      case HC_ASK_BRANDING_INFO => askBrandingInfoCodec.decode(bitVector)
      case HC_INIT_HOSTED_CHANNEL_TAG => initHostedChannelCodec.decode(bitVector)
      case HC_INVOKE_HOSTED_CHANNEL_TAG => invokeHostedChannelCodec.decode(bitVector)
      case HC_LAST_CROSS_SIGNED_STATE_TAG => lastCrossSignedStateCodec.decode(bitVector)
      case HC_ANNOUNCEMENT_SIGNATURE_TAG => announcementSignatureCodec.decode(bitVector)
      case HC_HOSTED_CHANNEL_BRANDING_TAG => hostedChannelBrandingCodec.decode(bitVector)
      case HC_QUERY_PUBLIC_HOSTED_CHANNELS_TAG => queryPublicHostedChannelsCodec.decode(bitVector)
      case HC_REPLY_PUBLIC_HOSTED_CHANNELS_END_TAG => replyPublicHostedChannelsEndCodec.decode(bitVector)
      case HC_QUERY_PREIMAGES_TAG => queryPreimagesCodec.decode(bitVector)
      case HC_REPLY_PREIMAGES_TAG => replyPreimagesCodec.decode(bitVector)
    }

    decodeAttempt.map(_.value)
  }

  def toUnknownHostedMessage(message: HostedChannelMessage): UnknownMessage = message match {
    case msg: StateUpdate => UnknownMessage(HC_STATE_UPDATE_TAG, stateUpdateCodec.encode(msg).require.toByteVector)
    case msg: StateOverride => UnknownMessage(HC_STATE_OVERRIDE_TAG, stateOverrideCodec.encode(msg).require.toByteVector)
    case msg: ResizeChannel => UnknownMessage(HC_RESIZE_CHANNEL_TAG, resizeChannelCodec.encode(msg).require.toByteVector)
    case msg: AskBrandingInfo => UnknownMessage(HC_ASK_BRANDING_INFO, askBrandingInfoCodec.encode(msg).require.toByteVector)
    case msg: InitHostedChannel => UnknownMessage(HC_INIT_HOSTED_CHANNEL_TAG, initHostedChannelCodec.encode(msg).require.toByteVector)
    case msg: InvokeHostedChannel => UnknownMessage(HC_INVOKE_HOSTED_CHANNEL_TAG, invokeHostedChannelCodec.encode(msg).require.toByteVector)
    case msg: LastCrossSignedState => UnknownMessage(HC_LAST_CROSS_SIGNED_STATE_TAG, lastCrossSignedStateCodec.encode(msg).require.toByteVector)
    case msg: AnnouncementSignature => UnknownMessage(HC_ANNOUNCEMENT_SIGNATURE_TAG, announcementSignatureCodec.encode(msg).require.toByteVector)
    case msg: HostedChannelBranding => UnknownMessage(HC_HOSTED_CHANNEL_BRANDING_TAG, hostedChannelBrandingCodec.encode(msg).require.toByteVector)
    case msg: QueryPublicHostedChannels => UnknownMessage(HC_QUERY_PUBLIC_HOSTED_CHANNELS_TAG, queryPublicHostedChannelsCodec.encode(msg).require.toByteVector)
    case msg: ReplyPublicHostedChannelsEnd => UnknownMessage(HC_REPLY_PUBLIC_HOSTED_CHANNELS_END_TAG, replyPublicHostedChannelsEndCodec.encode(msg).require.toByteVector)
    case msg: QueryPreimages => UnknownMessage(HC_QUERY_PREIMAGES_TAG, queryPreimagesCodec.encode(msg).require.toByteVector)
    case msg: ReplyPreimages => UnknownMessage(HC_REPLY_PREIMAGES_TAG, replyPreimagesCodec.encode(msg).require.toByteVector)
  }

  // Normal channel messages which are also used in HC

  def decodeHasChanIdMessage(wrap: UnknownMessage): Attempt[HasChannelId] = {
    val bitVector = wrap.data.toBitVector

    val decodeAttempt = wrap.tag match {
      case HC_ERROR_TAG => errorCodec.decode(bitVector)
      case HC_UPDATE_ADD_HTLC_TAG => updateAddHtlcCodec.decode(bitVector)
      case HC_UPDATE_FAIL_HTLC_TAG => updateFailHtlcCodec.decode(bitVector)
      case HC_UPDATE_FULFILL_HTLC_TAG => updateFulfillHtlcCodec.decode(bitVector)
      case HC_UPDATE_FAIL_MALFORMED_HTLC_TAG => updateFailMalformedHtlcCodec.decode(bitVector)
      case tag => Attempt failure Err(s"PLGN PHC, wrong tag=$tag")
    }

    decodeAttempt.map(_.value)
  }

  def toUnknownHasChanIdMessage(message: HasChannelId): UnknownMessage = message match {
    case msg: Error => UnknownMessage(HC_ERROR_TAG, errorCodec.encode(msg).require.toByteVector)
    case msg: UpdateAddHtlc => UnknownMessage(HC_UPDATE_ADD_HTLC_TAG, updateAddHtlcCodec.encode(msg).require.toByteVector)
    case msg: UpdateFailHtlc => UnknownMessage(HC_UPDATE_FAIL_HTLC_TAG, updateFailHtlcCodec.encode(msg).require.toByteVector)
    case msg: UpdateFulfillHtlc => UnknownMessage(HC_UPDATE_FULFILL_HTLC_TAG, updateFulfillHtlcCodec.encode(msg).require.toByteVector)
    case msg: UpdateFailMalformedHtlc => UnknownMessage(HC_UPDATE_FAIL_MALFORMED_HTLC_TAG, updateFailMalformedHtlcCodec.encode(msg).require.toByteVector)
    case msg => throw new RuntimeException(s"PLGN PHC, wrong message=${msg.getClass.getName}")
  }

  // Normal gossip messages which are also used in PHC gossip

  def decodeAnnounceMessage(wrap: UnknownMessage): Attempt[AnnouncementMessage] = {
    val bitVector = wrap.data.toBitVector

    val decodeAttempt = wrap.tag match {
      case PHC_ANNOUNCE_GOSSIP_TAG => channelAnnouncementCodec.decode(bitVector)
      case PHC_ANNOUNCE_SYNC_TAG => channelAnnouncementCodec.decode(bitVector)
      case PHC_UPDATE_GOSSIP_TAG => channelUpdateCodec.decode(bitVector)
      case PHC_UPDATE_SYNC_TAG => channelUpdateCodec.decode(bitVector)
      case tag => Attempt failure Err(s"PLGN PHC, wrong tag=$tag")
    }

    decodeAttempt.map(_.value)
  }

  def toUnknownAnnounceMessage(message: AnnouncementMessage, isGossip: Boolean): UnknownMessage = message match {
    case msg: ChannelAnnouncement if isGossip => UnknownMessage(PHC_ANNOUNCE_GOSSIP_TAG, channelAnnouncementCodec.encode(msg).require.toByteVector)
    case msg: ChannelAnnouncement => UnknownMessage(PHC_ANNOUNCE_SYNC_TAG, channelAnnouncementCodec.encode(msg).require.toByteVector)
    case msg: ChannelUpdate if isGossip => UnknownMessage(PHC_UPDATE_GOSSIP_TAG, channelUpdateCodec.encode(msg).require.toByteVector)
    case msg: ChannelUpdate => UnknownMessage(PHC_UPDATE_SYNC_TAG, channelUpdateCodec.encode(msg).require.toByteVector)
    case msg => throw new RuntimeException(s"PLGN PHC, wrong message=${msg.getClass.getName}")
  }
}
