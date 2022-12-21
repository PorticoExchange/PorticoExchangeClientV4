package fr.acinq.eclair.wire.internal.channel.version3

import fr.acinq.eclair.wire.internal.channel.version3.ChannelCodecs3.Codecs.{commitmentSpecCodec, originsMapCodec}
import fr.acinq.eclair.wire.internal.channel.version3.HCProtocolCodecs._
import fr.acinq.eclair.wire.protocol.CommonCodecs.{bool8, bytes32, lengthDelimited, publicKey}
import fr.acinq.eclair.wire.protocol.LightningMessageCodecs._
import fr.acinq.eclair.wire.protocol.{HasChannelId, UpdateMessage}
import fr.acinq.hc.app.channel.{ErrorExt, HC_DATA_ESTABLISHED, HostedCommitments, HostedState}
import scodec.codecs.{listOfN, optional, uint16, utf8, variableSizeBytes}
import scodec.{Attempt, Codec}


object HostedChannelCodecs {
  val updateMessageWithHasChannelIdCodec: Codec[UpdateMessage with HasChannelId] = lengthDelimited {
    lightningMessageCodec.narrow(Attempt successful _.asInstanceOf[UpdateMessage with HasChannelId], identity)
  }

  val hostedCommitmentsCodec = {
    (publicKey withContext "localNodeId") ::
      (publicKey withContext "remoteNodeId") ::
      (bytes32 withContext "channelId") ::
      (commitmentSpecCodec withContext "localSpec") ::
      (originsMapCodec withContext "originChannels") ::
      (lengthDelimited(lastCrossSignedStateCodec) withContext "lastCrossSignedState") ::
      (listOfN(uint16, updateMessageWithHasChannelIdCodec) withContext "nextLocalUpdates") ::
      (listOfN(uint16, updateMessageWithHasChannelIdCodec) withContext "nextRemoteUpdates") ::
      (bool8 withContext "announceChannel")
  }.as[HostedCommitments]

  val errorExtCodec = {
    (lengthDelimited(errorCodec) withContext "localError") ::
      (variableSizeBytes(uint16, utf8) withContext "stamp") ::
      (variableSizeBytes(uint16, utf8) withContext "description")
  }.as[ErrorExt]

  val HC_DATA_ESTABLISHED_Codec = {
    (hostedCommitmentsCodec withContext "commitments") ::
      (lengthDelimited(channelUpdateCodec) withContext "channelUpdate") ::
      (listOfN(uint16, errorExtCodec) withContext "localErrors") ::
      (optional(bool8, errorExtCodec) withContext "remoteError") ::
      (optional(bool8, lengthDelimited(resizeChannelCodec)) withContext "resizeProposal") ::
      (optional(bool8, lengthDelimited(stateOverrideCodec)) withContext "overrideProposal") ::
      (optional(bool8, lengthDelimited(channelAnnouncementCodec)) withContext "channelAnnouncement")
  }.as[HC_DATA_ESTABLISHED]

  val hostedStateCodec = {
    (publicKey withContext "nodeId1") ::
      (publicKey withContext "nodeId2") ::
      (lastCrossSignedStateCodec withContext "lastCrossSignedState")
  }.as[HostedState]
}
