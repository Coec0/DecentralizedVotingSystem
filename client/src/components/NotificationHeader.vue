<template>
	<div class="notifications">
		<div v-for="item in items" class="notification">
			{{ item.message }}
		</div>
	</div>
</template>

<script>
export default {
	name: 'NotificationHeader',
	props: {
		items: Array
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
@keyframes slideInFromTop {
	0% {
		transform: translateY(-100%);
	}
	100% {
		transform: translateY(0);
	}
}

.notifications {
	width: 100%;
	animation-duration: 1s; /* the duration of the animation */
	animation-timing-function: ease-out; /* how the animation will behave */
	animation-delay: 0s; /* how long to delay the animation from starting */
	animation-iteration-count: 1; /* how many times the animation will play */
	animation-name: slideInFromTop; /* the name of the animation we defined above */
}
.notification {
	padding: 2px 0px;
	background-color: #ffcc00;
	border-bottom: 1px solid rgba(0, 0, 0, 0.2);
}
</style>
