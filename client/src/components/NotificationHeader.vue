<template>
	<div id="notifications">
		<div v-for="item in items" :key="item.message" class="notification" v-bind:class="item.type">
			{{ item.message }}
		</div>
	</div>
</template>

<script>
export default {
	name: 'NotificationHeader',
	data() {
		return {
			items: this.$store.state.notifications
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
#notifications {
	text-align: center;
	line-height: 1.5;
	overflow: hidden; 
}

.notification {
	animation-duration: 1s;
	animation-name: slidein;
	padding: 2px 0px;
	border-bottom: 1px solid rgba(0, 0, 0, 0.5);
}

@keyframes slidein {
  from {
  	transform: translateY(-50px);
  }

  to {
    transform: translateY(0px);
  }
}

.notify {
	background-color: #ffcc00;
}

.warn {
	background-color: #B33A3A;
}
</style>
