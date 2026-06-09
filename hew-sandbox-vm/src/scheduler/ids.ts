export class DeterministicIds {
  private actorCounter = 0;
  private messageCounter = 0;
  private replyCounter = 0;
  private channelCounter = 0;
  private taskCounter = 0;
  private supervisorCounter = 0;
  private monitorCounter = 0;

  constructor(readonly seed: number) {}

  actor(): string {
    this.actorCounter += 1;
    return `actor:a${this.actorCounter}`;
  }

  message(): string {
    this.messageCounter += 1;
    return `message:m${this.messageCounter}`;
  }

  reply(): string {
    this.replyCounter += 1;
    return `reply:r${this.replyCounter}`;
  }

  channel(): string {
    this.channelCounter += 1;
    return `channel:c${this.channelCounter}`;
  }

  task(): string {
    this.taskCounter += 1;
    return `task:t${this.taskCounter}`;
  }

  supervisor(): string {
    this.supervisorCounter += 1;
    return `supervisor:s${this.supervisorCounter}`;
  }

  monitor(): string {
    this.monitorCounter += 1;
    return `monitor:m${this.monitorCounter}`;
  }
}
